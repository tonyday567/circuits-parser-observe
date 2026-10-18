{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | jsonl-micro: floor-vs-machine, pinned down by ladder.
--
-- The first run of this suite showed the per-byte machine *faster* than the
-- shipped floor lexer (11.5 vs 14.0 ns/B), which contradicted the "floor is
-- the lower bound" story. This version decomposes that delta: a ladder of
-- engines, each removing exactly one mechanism of the shipped floor, all
-- producing identical token output:
--
--   floor       — 'runJsonLexerBS' as shipped: Either fmap consing,
--                 literal validation via temp slice + compare.
--   floor-nolit — same loop, literals counted not validated (machine parity
--                 on that axis). delta = literal validation cost.
--   floor-acc   — same scans, strict accumulator + reverse, no Either.
--                 delta = Either/lazy-cons cost.
--   machine     — per-byte Cell/Step state machine. delta = iteration
--                 granularity (token-recursion vs per-byte).
--   floor-scan  — count-only traversal: no slices, no tokens, no list.
--                 Brackets materialization cost: floor-acc - floor-scan.
--
-- Corpus: one wire-flavoured line of exactly 50 tokens; shapes are round
-- token counts (1000/2000/4000/8000) so scaling is visible per-token.
--
-- Usage: jsonl-micro                    — conservation, then timing ladder
--        jsonl-micro alloc ENGINE TOKS  — single run (for +RTS -s)
module Main (main) where

import Circuit.Category (K (..))
import Circuit.Machine (Cell (..), Step (..))
import Circuit.Meter.Time (Nanos, ticks)
import Circuit.Parser.Json (JsonToken (..), runJsonLexerBS)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Unsafe qualified as BSU
import Data.Functor.Identity (Identity (..))
import Data.Word (Word8)
import System.Environment (getArgs)
import System.IO (hPrint, stderr)
import Text.Printf (printf)

-- * corpus: exactly 100 tokens per line pair

--
-- A single JSON value always tokenizes to an odd count (atoms are 1, and
-- object/array construction preserves parity), so round token totals use an
-- A/B line pair: 51 + 49 = 100 tokens.

tokensPerPair :: Int
tokensPerPair = 100

lineA, lineB :: ByteString
lineA =
  C8.pack
    "{\"type\":\"context.append_loop_event\",\"event\":{\"type\":\"content.part\",\"part\":{\"type\":\"text\",\"text\":\"agent text with \\\"quoted\\\" bits\",\"tokens\":1234,\"cost\":0.0047},\"ok\":true,\"items\":[1,2,3],\"n\":null}}"
lineB =
  C8.pack
    "{\"type\":\"context.append_loop_event\",\"event\":{\"type\":\"content.part\",\"part\":{\"type\":\"text\",\"text\":\"agent text with \\\"quoted\\\" bits\",\"tokens\":1234,\"cost\":0.0047},\"ok\":true,\"items\":[1,2],\"n\":null}}"

-- | Build a corpus with an exact round token count.
jsonl :: Int -> ByteString
jsonl toks
  | toks `mod` tokensPerPair /= 0 = error "token count must be a multiple of 100"
  | otherwise = BS.concat (replicate (toks `div` tokensPerPair) (lineA <> nl <> lineB <> nl))
  where
    nl = C8.pack "\n"

shapes :: [(String, Int, ByteString)]
shapes =
  [ ("jsonl-" <> show t, t, jsonl t)
  | t <- [1000, 2000, 4000, 8000]
  ]

-- * shared slicer (used by the machine)

sliceBS :: ByteString -> Int -> Int -> ByteString
sliceBS bs s e = BSU.unsafeTake (e - s) (BSU.unsafeDrop s bs)

-- * ladder step 1b: verbatim local copy of the shipped floor

--
-- Anchor for compilation drift: identical code, identical helper structure
-- (local where-closures over bs/len). An earlier "copy" that factored the
-- span scanners out to top-level functions ran ~30% slower — helper
-- structure, not module boundaries, is what moves the lazy variants.

lexFloorVerbatim :: ByteString -> Either (Int, String) [JsonToken]
lexFloorVerbatim bs = go 0
  where
    !len = BS.length bs
    go !i
      | i >= len = Right []
      | otherwise = case BSU.unsafeIndex bs i of
          w
            | isSpaceW w -> go (i + 1)
          0x7B -> emit TBraceOpen i
          0x7D -> emit TBraceClose i
          0x5B -> emit TBrackOpen i
          0x5D -> emit TBrackClose i
          0x2C -> emit TComma i
          0x3A -> emit TColon i
          0x22 -> stringAt (i + 1)
          0x74 -> literalAt i "rue" TTrue
          0x66 -> literalAt i "alse" TFalse
          0x6E -> literalAt i "ull" TNull
          w
            | w == 0x2D || isDigitW w -> numberAt i
            | otherwise -> Left (i, "unexpected byte " ++ show w)
    emit t i = (t :) <$> go (i + 1)
    stringAt !i = case stringEnd i of
      Nothing -> Left (len, "unterminated string")
      Just j -> (TString (slice i j) :) <$> go (j + 1)
    stringEnd !i
      | i >= len = Nothing
      | otherwise = case BSU.unsafeIndex bs i of
          0x5C -> if i + 1 >= len then Nothing else stringEnd (i + 2)
          0x22 -> Just i
          _ -> stringEnd (i + 1)
    numberAt !i =
      let !j = numberEnd i
       in (TNumber (slice i j) :) <$> go j
    numberEnd !i
      | i >= len = i
      | otherwise =
          let w = BSU.unsafeIndex bs i
           in if isNumW w then numberEnd (i + 1) else i
    literalAt !i rest t
      | i + 1 + BS.length rest <= len && BSU.unsafeTake (BS.length rest) (BSU.unsafeDrop (i + 1) bs) == rest =
          (t :) <$> go (i + 1 + BS.length rest)
      | otherwise = Left (i, "bad literal")
    slice s e = BSU.unsafeTake (e - s) (BSU.unsafeDrop s bs)

-- * ladder step 2: floor minus literal validation (counted, machine parity)

lexNoLit :: ByteString -> [JsonToken]
lexNoLit bs = either (error "lexNoLit failed") id (go 0)
  where
    !len = BS.length bs
    go :: Int -> Either (Int, String) [JsonToken]
    go !i
      | i >= len = Right []
      | otherwise = case BSU.unsafeIndex bs i of
          w
            | isSpaceW w -> go (i + 1)
          0x7B -> emit TBraceOpen i
          0x7D -> emit TBraceClose i
          0x5B -> emit TBrackOpen i
          0x5D -> emit TBrackClose i
          0x2C -> emit TComma i
          0x3A -> emit TColon i
          0x22 -> stringAt (i + 1)
          0x74 -> emitAt TTrue i 4
          0x66 -> emitAt TFalse i 5
          0x6E -> emitAt TNull i 4
          w
            | w == 0x2D || isDigitW w -> numberAt i
            | otherwise -> Left (i, "unexpected byte " ++ show w)
    emit t i = (t :) <$> go (i + 1)
    emitAt t i n = (t :) <$> go (i + n)
    stringAt !i = case stringEnd i of
      Nothing -> Left (len, "unterminated string")
      Just j -> (TString (slice i j) :) <$> go (j + 1)
    stringEnd !i
      | i >= len = Nothing
      | otherwise = case BSU.unsafeIndex bs i of
          0x5C -> if i + 1 >= len then Nothing else stringEnd (i + 2)
          0x22 -> Just i
          _ -> stringEnd (i + 1)
    numberAt !i =
      let !j = numberEnd i
       in (TNumber (slice i j) :) <$> go j
    numberEnd !i
      | i >= len = i
      | otherwise =
          let w = BSU.unsafeIndex bs i
           in if isNumW w then numberEnd (i + 1) else i
    slice s e = BSU.unsafeTake (e - s) (BSU.unsafeDrop s bs)

-- * ladder step 3: minus Either/lazy consing (strict accumulator + reverse)

lexAcc :: ByteString -> [JsonToken]
lexAcc bs = go 0 []
  where
    !len = BS.length bs
    go !i !acc
      | i >= len = reverse acc
      | otherwise = case BSU.unsafeIndex bs i of
          w
            | isSpaceW w -> go (i + 1) acc
          0x7B -> go (i + 1) (TBraceOpen : acc)
          0x7D -> go (i + 1) (TBraceClose : acc)
          0x5B -> go (i + 1) (TBrackOpen : acc)
          0x5D -> go (i + 1) (TBrackClose : acc)
          0x2C -> go (i + 1) (TComma : acc)
          0x3A -> go (i + 1) (TColon : acc)
          0x22 -> case stringEnd (i + 1) of
            Just j -> go (j + 1) (TString (slice (i + 1) j) : acc)
            Nothing -> error "unterminated string"
          0x74 -> go (i + 4) (TTrue : acc)
          0x66 -> go (i + 5) (TFalse : acc)
          0x6E -> go (i + 4) (TNull : acc)
          w
            | w == 0x2D || isDigitW w ->
                let !j = numberEnd i
                 in go j (TNumber (slice i j) : acc)
            | otherwise -> error "unexpected byte"
    stringEnd !i
      | i >= len = Nothing
      | otherwise = case BSU.unsafeIndex bs i of
          0x5C -> if i + 1 >= len then Nothing else stringEnd (i + 2)
          0x22 -> Just i
          _ -> stringEnd (i + 1)
    numberEnd !i
      | i >= len = i
      | otherwise =
          let w = BSU.unsafeIndex bs i
           in if isNumW w then numberEnd (i + 1) else i
    slice s e = BSU.unsafeTake (e - s) (BSU.unsafeDrop s bs)

-- * ladder diagnostic: count-only traversal, no materialization

lexCount :: ByteString -> Int
lexCount bs = go 0 0
  where
    !len = BS.length bs
    go !i !n
      | i >= len = n
      | otherwise = case BSU.unsafeIndex bs i of
          w
            | isSpaceW w -> go (i + 1) n
          0x22 -> case stringEnd (i + 1) of
            Just j -> go (j + 1) (n + 1)
            Nothing -> error "unterminated string"
          0x74 -> go (i + 4) (n + 1)
          0x66 -> go (i + 5) (n + 1)
          0x6E -> go (i + 4) (n + 1)
          w
            | w == 0x2D || isDigitW w -> go (numberEnd i) (n + 1)
            | otherwise -> go (i + 1) (n + 1)
    stringEnd !i
      | i >= len = Nothing
      | otherwise = case BSU.unsafeIndex bs i of
          0x5C -> if i + 1 >= len then Nothing else stringEnd (i + 2)
          0x22 -> Just i
          _ -> stringEnd (i + 1)
    numberEnd !i
      | i >= len = i
      | otherwise =
          let w = BSU.unsafeIndex bs i
           in if isNumW w then numberEnd (i + 1) else i

-- * ladder step 4: the machine

data Ctx = CStruct | CString | CStringEsc | CNumber | CLit !Int JsonToken

data St = St !Ctx !Int !Int

-- | Emission discipline: a per-tick machine has no pushback, so the byte
-- that terminates a number must also be processed — 'E2' carries the
-- number token and the structural token it ended on.
data Em = E0 | E1 JsonToken | E2 JsonToken JsonToken

cell :: ByteString -> Cell (,) (,) St St (K Identity) (Word8, Int) Em
cell src =
  Cell
    (K (\() -> Identity (St CStruct 0 0)))
    (Step (K (\(st, wi) -> Identity (mstep src (st, wi)))))

mstep :: ByteString -> (St, (Word8, Int)) -> (St, Em)
mstep src (St c s _, (w, i)) = case (c, w) of
  (CStruct, 0x7B) -> atom TBraceOpen
  (CStruct, 0x7D) -> atom TBraceClose
  (CStruct, 0x5B) -> atom TBrackOpen
  (CStruct, 0x5D) -> atom TBrackClose
  (CStruct, 0x2C) -> atom TComma
  (CStruct, 0x3A) -> atom TColon
  (CStruct, 0x22) -> (St CString (i + 1) 0, E0)
  (CStruct, 0x74) -> (St (CLit 3 TTrue) 0 0, E0)
  (CStruct, 0x66) -> (St (CLit 4 TFalse) 0 0, E0)
  (CStruct, 0x6E) -> (St (CLit 3 TNull) 0 0, E0)
  (CStruct, _)
    | isNumW w -> (St CNumber i 0, E0)
    | otherwise -> (St CStruct 0 0, E0)
  (CString, 0x5C) -> (St CStringEsc s 0, E0)
  (CString, 0x22) -> (St CStruct 0 0, E1 (TString (sliceBS src s i)))
  (CString, _) -> (St CString s 0, E0)
  (CStringEsc, _) -> (St CString s 0, E0)
  (CLit 1 tok, _) -> (St CStruct 0 0, E1 tok)
  (CLit n tok, _) -> (St (CLit (n - 1) tok) 0 0, E0)
  (CNumber, _)
    | isNumW w -> (St CNumber s 0, E0)
    | isSpaceW w -> (St CStruct 0 0, E1 (TNumber (sliceBS src s i)))
    | otherwise -> case structTok w of
        Just t -> (St CStruct 0 0, E2 (TNumber (sliceBS src s i)) t)
        Nothing -> error "bad number delimiter"
  where
    atom t = (St CStruct 0 0, E1 t)

-- | Hand-drive the Cell over the bytes; the string/number slices are cut
-- against the source inside the step.
runMachine :: ByteString -> [JsonToken]
runMachine src =
  let Cell (K seed) (Step (K stf)) = cell src
      s0 = runIdentity (seed ())
      go !st !i !acc
        | i >= len = reverse (flush st acc)
        | otherwise = case runIdentity (stf (st, (BSU.unsafeIndex src i, i))) of
            (st', e) -> go st' (i + 1) (push e acc)
      push e acc = case e of
        E0 -> acc
        E1 t -> t : acc
        E2 t u -> u : t : acc
      flush (St cst s _) acc = case cst of
        CNumber -> TNumber (sliceBS src s len) : acc
        _ -> acc
   in go s0 0 []
  where
    !len = BS.length src

-- * predicates

isNumW, isSpaceW, isDigitW :: Word8 -> Bool
isNumW w = (w >= 0x30 && w <= 0x39) || w == 0x2D || w == 0x2B || w == 0x2E || w == 0x65 || w == 0x45
isSpaceW w = w == 0x20 || w == 0x0A || w == 0x0D || w == 0x09
isDigitW w = w >= 0x30 && w <= 0x39

structTok :: Word8 -> Maybe JsonToken
structTok = \case
  0x7B -> Just TBraceOpen
  0x7D -> Just TBraceClose
  0x5B -> Just TBrackOpen
  0x5D -> Just TBrackClose
  0x2C -> Just TComma
  0x3A -> Just TColon
  _ -> Nothing

-- * harness

engines :: [(String, ByteString -> [JsonToken])]
engines =
  [ ("floor", either (error "floor lexer failed") id . runJsonLexerBS),
    ("floor-verb", either (error "floor-verb failed") id . lexFloorVerbatim),
    ("floor-nolit", lexNoLit),
    ("floor-acc", lexAcc),
    ("machine", runMachine)
  ]

scans :: [(String, ByteString -> Int)]
scans = [("floor-scan", lexCount)]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["alloc", eng, toks] -> allocMode eng (read toks)
    _ -> benchAll

allocMode :: String -> Int -> IO ()
allocMode eng toks =
  let src = jsonl toks
   in case (lookup eng engines, lookup eng scans) of
        (Just f, Nothing) -> hPrint stderr (BS.length src) >> print (length (f src))
        (Nothing, Just f) -> hPrint stderr (BS.length src) >> print (f src)
        _ -> error "usage: jsonl-micro alloc ENGINE TOKENS"

-- | Reps per shape, scaled so each measurement is ~30ms minimum.
repsFor :: Int -> Int
repsFor toks = max 30 (200000 `div` (toks `div` 10))

-- | Minimum (pure CPU, GC-lightest rep) and mean (GC amortised in) of
-- per-rep timings.
statsOf :: Int -> (a -> b) -> a -> IO (Nanos, Nanos)
statsOf n f a = do
  (ts, _) <- ticks n f a
  pure (minimum ts, sum ts `div` fromIntegral n)

benchAll :: IO ()
benchAll = do
  mapM_ conserve shapes
  putStrLn "conservation: exact (all token engines agree, scans count the same)"
  warmup
  putStrLn "shape            bytes    engine   ns/tok-min  ns/tok-mean    ns/B-min"
  forM_ shapes $ \(label, toks, src) -> do
    let row eng (nsMin, nsMean) =
          printf
            "%-14s %8d %-12s %10.3f %10.3f %10.3f\n"
            label
            (BS.length src)
            eng
            (fromIntegral nsMin / fromIntegral toks :: Double)
            (fromIntegral nsMean / fromIntegral toks :: Double)
            (fromIntegral nsMin / fromIntegral (BS.length src) :: Double)
    forM_ engines $ \(eng, f) -> statsOf (repsFor toks) (length . f) src >>= row eng
    forM_ scans $ \(eng, f) -> statsOf (repsFor toks) f src >>= row eng

warmup :: IO ()
warmup =
  forM_ shapes $ \(_, _, src) -> do
    forM_ engines $ \(_, f) -> statsOf 3 (length . f) src
    forM_ scans $ \(_, f) -> statsOf 3 f src

conserve :: (String, Int, ByteString) -> IO ()
conserve (label, toks, src) = case engines of
  [] -> pure ()
  ((e0, f0) : rest) -> do
    let r0 = f0 src
    if length r0 /= toks
      then error ("corpus off on " <> label <> ": expected " <> show toks <> " tokens, got " <> show (length r0))
      else
        mapM_
          (\(e, f) -> if f src == r0 then pure () else error ("conservation broken on " <> label <> ": " <> e <> " vs " <> e0))
          rest
    mapM_
      (\(e, f) -> if f src == toks then pure () else error ("scan count off on " <> label <> ": " <> e))
      scans
