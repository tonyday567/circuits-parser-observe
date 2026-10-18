{-# LANGUAGE BangPatterns #-}

-- | Shared jsonl tokenizing engines — lifted from bench/Jsonl.hs
-- (circuits-parser-machina 94f79f4) so jsonl-micro and wires-micro ride
-- one machine. The INLINE pragmas on 'jsonCell' and 'mstep' are
-- load-bearing: with more than one runner, GHC stops specializing the
-- loop without them (loom/perf.md §the INLINE trap).
module Engines
  ( -- * walk-only diagnostic
    lexCount,

    -- * the machine runners
    runJsonMachineBS,
    runJsonMachineIO,
  )
where

import Circuit.Category (K (..), runK)
import Circuit.Machine (Cell (..), Step (..))
import Circuit.Parser.Json (JsonToken (..))
import Circuit.Traced (Yank (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Functor.Identity (Identity (..))
import Data.Word (Word8)

-- * shared slicer (used by the machine)

sliceBS :: ByteString -> Int -> Int -> ByteString
sliceBS bs s e = BSU.unsafeTake (e - s) (BSU.unsafeDrop s bs)

-- * walk-only diagnostic: count-only traversal, no materialization

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

-- * the machine

--
-- The Cell is input-independent: emissions carry spans (start, end) into
-- the source, and the runner — which holds the source — cuts the slices.

-- | One emission, flat: nothing, an atom, a span to slice, or a span plus
-- the structural token that terminated it.
data Em = E0 | EAtom JsonToken | EStr !Int !Int | ENum !Int !Int | ENumStruct !Int !Int JsonToken

data Ctx = CStruct | CString | CStringEsc | CNumber | CLit !Int JsonToken

data St = St !Ctx !Int !Int

jsonCell :: Cell (,) (,) St St (K Identity) (Word8, Int) Em
jsonCell =
  Cell
    (K (\() -> Identity (St CStruct 0 0)))
    (Step (K (Identity . mstep)))
{-# INLINE jsonCell #-}

-- | Emission discipline: a per-tick machine has no pushback, so the byte
-- that terminates a number must also be processed — 'ENumStruct' carries
-- the number span and the structural token it ended on.
mstep :: (St, (Word8, Int)) -> (St, Em)
mstep (St c s _, (w, i)) = case (c, w) of
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
  (CString, 0x22) -> (St CStruct 0 0, EStr s i)
  (CString, _) -> (St CString s 0, E0)
  (CStringEsc, _) -> (St CString s 0, E0)
  (CLit 1 tok, _) -> (St CStruct 0 0, EAtom tok)
  (CLit n tok, _) -> (St (CLit (n - 1) tok) 0 0, E0)
  (CNumber, _)
    | isNumW w -> (St CNumber s 0, E0)
    | isSpaceW w -> (St CStruct 0 0, ENum s i)
    | otherwise -> case structTok w of
        Just t -> (St CStruct 0 0, ENumStruct s i t)
        Nothing -> error "bad number delimiter"
  where
    atom t = (St CStruct 0 0, EAtom t)
{-# INLINE mstep #-}

-- * runner-shared materialization

push :: ByteString -> Em -> [JsonToken] -> [JsonToken]
push src e acc = case e of
  E0 -> acc
  EAtom t -> t : acc
  EStr s e' -> TString (sliceBS src s e') : acc
  ENum s e' -> TNumber (sliceBS src s e') : acc
  ENumStruct s e' t -> t : TNumber (sliceBS src s e') : acc
{-# INLINE push #-}

flush :: ByteString -> Int -> St -> [JsonToken] -> [JsonToken]
flush src len (St cst s _) acc = case cst of
  CNumber -> TNumber (sliceBS src s len) : acc
  _ -> acc

-- | Hand-drive 'jsonCell' over the bytes, cutting span slices as they emit.
runJsonMachineBS :: ByteString -> [JsonToken]
runJsonMachineBS src = go s0 0 []
  where
    !len = BS.length src
    Cell (K seed) (Step (K stf)) = jsonCell
    s0 = runIdentity (seed ())
    go !st !i !acc
      | i >= len = reverse (flush src len st acc)
      | otherwise = case runIdentity (stf (st, (BSU.unsafeIndex src i, i))) of
          (st', e) -> go st' (i + 1) (push src e acc)

-- | Strict loop carrier for the yank runner: no per-iteration thunk chains.
data Loop = Loop !St !Int [JsonToken]

-- | 'jsonCell' driven through 'yank' at the Either tensor over K IO —
-- the delimited-continuation runner from Circuit.Traced. Each iteration
-- re-enters a prompt; a Left fires control0, capturing the continuation
-- and jumping back — constant stack, but every byte crosses the boundary.
runJsonMachineIO :: ByteString -> IO [JsonToken]
runJsonMachineIO src = runK (yank body) ()
  where
    !len = BS.length src
    Cell (K seed) (Step (K stf)) = jsonCell
    s0 = runIdentity (seed ())
    body :: K IO (Either Loop ()) (Either Loop [JsonToken])
    body =
      K $ \case
        Right () -> pure (Left (Loop s0 0 []))
        Left (Loop st i acc)
          | i >= len -> pure (Right (reverse (flush src len st acc)))
          | otherwise ->
              case runIdentity (stf (st, (BSU.unsafeIndex src i, i))) of
                (st', e) -> pure (Left (Loop st' (i + 1) (push src e acc)))

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
