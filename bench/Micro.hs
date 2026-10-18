{-# LANGUAGE BangPatterns #-}

-- | parser-micro: the markup tokenize problem, extracted to a minimal grammar.
--
-- One grammar, four engines, identical token output (conservation is the
-- falsifier). The grammar is markup reduced to its cost skeleton:
--
-- @
--   token   := content | open | empty | close
--   content := [^\<]+                      TContent slice
--   open    := '\<' name attr* '\>'        TOpen name
--   empty   := '\<' name attr* '/' '\>'    TEmpty name
--   close   := '\<' '/' name '\>'          TClose name
--   name    := [a-z][a-z0-9]*
--   attr    := ' '+ name '=' '"' [^\"]* '"'
-- @
--
-- The engines:
--
--   naive     — Circuit.Parser combinators in the markup-parse shape:
--               string-prefix alternatives, content last, and StartTag
--               tried before EmptyTag so every empty element parses twice.
--   factored  — the same combinators, grammar factored: dispatch on '<'
--               then '/', name and attrs parsed once, branch at the end.
--   machine   — the recognizer as a bare Circuit.Machine Cell/Step over
--               (Word8, offset), Maybe token emissions, hand-driven.
--   floor     — one recursive unsafeIndex loop, zero-copy slices.
--
-- naive/factored prices grammar shape; factored/machine prices the
-- combinator interpretation; machine/floor prices the stream abstraction.
--
-- Usage: parser-micro            — conservation check, then ticksN table
--        parser-micro alloc SHAPE ENGINE  — single run (for +RTS -s)
module Main (main) where

import Circuit.Category (K (..))
import Circuit.Machine (Cell (..), Step (..))
import Circuit.Meter.Time (ticksN)
import Circuit.Parser
  ( Parser,
    asThese,
    bs,
    char,
    many,
    runParserIdentity,
    satisfy,
    skipWhile,
    some,
    string,
    (<|>),
  )
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Unsafe qualified as BSU
import Data.Char (isAsciiLower, isDigit)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.Word (Word8)
import System.Environment (getArgs)
import Text.Printf (printf)

-- * the token type

data Tok
  = TContent ByteString
  | TOpen ByteString
  | TClose ByteString
  | TEmpty ByteString
  deriving (Eq, Show)

-- * corpus

flatC, openC, attrC, contentC :: Int -> ByteString
flatC n = pk "<r>" <> C8.concat (replicate n (pk "<i/>")) <> pk "</r>"
openC n = pk "<r>" <> C8.concat (replicate n (pk "<a>x</a>")) <> pk "</r>"
attrC n = pk "<r>" <> C8.concat (replicate n el) <> pk "</r>"
  where
    el = pk "<e" <> C8.concat [pk " k" <> C8.pack (show j) <> pk "=\"v\"" | j <- [1 .. 4 :: Int]] <> pk "/>"
contentC n = pk "<r>" <> C8.concat (replicate n (pk "<p>" <> C8.replicate 40 'x' <> pk "</p>")) <> pk "</r>"

pk :: String -> ByteString
pk = C8.pack

shapes :: [(String, ByteString)]
shapes =
  [ ("flat-500", flatC 500),
    ("open-500", openC 500),
    ("attr-500", attrC 500),
    ("content-500", contentC 500)
  ]

-- * engine 1: naive combinators (the markup-parse shape)

isNameLead, isNameChar', isSp :: Char -> Bool
isNameLead = isAsciiLower
isNameChar' c = isNameLead c || isDigit c
isSp c = c == ' '

nameP :: Parser Identity ByteString Char ByteString
nameP = bs (satisfy isNameLead *> many (satisfy isNameChar'))

contentP :: Parser Identity ByteString Char Tok
contentP = TContent <$> bs (some (satisfy (/= '<')))

attrP :: Parser Identity ByteString Char ByteString
attrP = bs (some (satisfy isNameChar')) <* char '=' <* (char '"' *> bs (many (satisfy (/= '"'))) <* char '"')

attrsP :: Parser Identity ByteString Char [ByteString]
attrsP = many (skipWhile isSp *> attrP) <* skipWhile isSp

tokenP1 :: Parser Identity ByteString Char Tok
tokenP1 = (string "</" *> closeP1) <|> (string "<" *> tagP1) <|> contentP

closeP1 :: Parser Identity ByteString Char Tok
closeP1 = TClose <$> (nameP <* char '>')

tagP1 :: Parser Identity ByteString Char Tok
tagP1 =
  (TOpen <$> (nameP <* attrsP) <* char '>')
    <|> (TEmpty <$> (nameP <* attrsP) <* string "/>")

-- * engine 2: factored combinators (same engine, good grammar)

tokenP2 :: Parser Identity ByteString Char Tok
tokenP2 = (char '<' *> afterLt) <|> contentP

afterLt :: Parser Identity ByteString Char Tok
afterLt = (char '/' *> (TClose <$> (nameP <* char '>'))) <|> tagP2

tagP2 :: Parser Identity ByteString Char Tok
tagP2 = (nameP <* attrsP) >>= \n -> (char '>' $> TOpen n) <|> (string "/>" $> TEmpty n)

-- * combinators runner

parseWith :: Parser Identity ByteString Char Tok -> ByteString -> [Tok]
parseWith p = asThese . runParserIdentity (many p)

-- * engine 3: bare Circuit.Machine Cell/Step

data Ctx = CContent | CName | CAttr | CAttrQ | CClose | CEmptyW

data St = St !Ctx !Int !Int

cell :: ByteString -> Cell (,) (,) St St (K Identity) (Word8, Int) (Maybe Tok)
cell src =
  Cell
    (K (\() -> Identity (St CContent 0 0)))
    (Step (K (\(st, wi) -> Identity (mstep src (st, wi)))))

mstep :: ByteString -> (St, (Word8, Int)) -> (St, Maybe Tok)
mstep src (St c s l, (w, i)) = case (c, w) of
  (CContent, 60) -> (St CName (i + 1) 0, emit s l)
  (CContent, _) -> (St CContent (if l == 0 then i else s) (l + 1), Nothing)
  (CName, 32) -> (St CAttr s l, Nothing)
  (CName, 62) -> (St CContent (i + 1) 0, Just (TOpen (slice s l)))
  (CName, 47)
    | l == 0 -> (St CClose (i + 1) 0, Nothing)
    | otherwise -> (St CEmptyW s l, Nothing)
  (CName, _) -> (St CName (if l == 0 then i else s) (l + 1), Nothing)
  (CAttr, 62) -> (St CContent (i + 1) 0, Just (TOpen (slice s l)))
  (CAttr, 47) -> (St CEmptyW s l, Nothing)
  (CAttr, 34) -> (St CAttrQ s l, Nothing)
  (CAttr, _) -> (St CAttr s l, Nothing)
  (CAttrQ, 34) -> (St CAttr s l, Nothing)
  (CAttrQ, _) -> (St CAttrQ s l, Nothing)
  (CEmptyW, 62) -> (St CContent (i + 1) 0, Just (TEmpty (slice s l)))
  (CEmptyW, _) -> (St CAttr s l, Nothing)
  (CClose, 62) -> (St CContent (i + 1) 0, Just (TClose (slice s l)))
  (CClose, _) -> (St CClose (if l == 0 then i else s) (l + 1), Nothing)
  where
    emit st ln = if ln == 0 then Nothing else Just (TContent (slice st ln))
    slice st ln = BSU.unsafeTake ln (BSU.unsafeDrop st src)

runMachine :: ByteString -> [Tok]
runMachine src =
  let Cell (K seed) (Step (K stf)) = cell src
      s0 = runIdentity (seed ())
      go !st !i !acc
        | i >= len = reverse acc
        | otherwise = case runIdentity (stf (st, (BSU.unsafeIndex src i, i))) of
            (st', mt) -> go st' (i + 1) (maybe acc (: acc) mt)
   in go s0 0 []
  where
    !len = BS.length src

-- * engine 4: the floor — one recursive unsafeIndex loop

lexFloor :: ByteString -> [Tok]
lexFloor src = goContent 0 0 0
  where
    !len = BS.length src
    slice s l = BSU.unsafeTake l (BSU.unsafeDrop s src)
    goContent !s !l !i
      | i >= len = [TContent (slice s l) | l > 0]
      | BSU.unsafeIndex src i == 60 =
          (if l > 0 then (TContent (slice s l) :) else id) (dispatch (i + 1))
      | otherwise = goContent (if l == 0 then i else s) (l + 1) (i + 1)
    dispatch j
      | BSU.unsafeIndex src j == 47 = goClose (j + 1) 0 (j + 1)
      | otherwise = goName j 0 j
    goName !s !l !i = case BSU.unsafeIndex src i of
      62 -> TOpen (slice s l) : goContent (i + 1) 0 (i + 1)
      47 -> TEmpty (slice s l) : goContent (i + 2) 0 (i + 2)
      32 -> goAttr s l (i + 1)
      _ -> goName (if l == 0 then i else s) (l + 1) (i + 1)
    goClose !s !l !i = case BSU.unsafeIndex src i of
      62 -> TClose (slice s l) : goContent (i + 1) 0 (i + 1)
      _ -> goClose (if l == 0 then i else s) (l + 1) (i + 1)
    goAttr !s !l !i = case BSU.unsafeIndex src i of
      62 -> TOpen (slice s l) : goContent (i + 1) 0 (i + 1)
      47 -> TEmpty (slice s l) : goContent (i + 2) 0 (i + 2)
      34 -> goAttrQ s l (i + 1)
      _ -> goAttr s l (i + 1)
    goAttrQ !s !l !i = case BSU.unsafeIndex src i of
      34 -> goAttr s l (i + 1)
      _ -> goAttrQ s l (i + 1)

-- * harness

engines :: [(String, ByteString -> [Tok])]
engines =
  [ ("naive", parseWith tokenP1),
    ("factored", parseWith tokenP2),
    ("machine", runMachine),
    ("floor", lexFloor)
  ]

reps :: Int
reps = 30

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["alloc", shape, eng] -> allocMode shape eng
    _ -> benchAll

allocMode :: String -> String -> IO ()
allocMode shape eng = case (lookup shape shapes, lookup eng engines) of
  (Just src, Just f) -> print (length (f src))
  _ -> error "usage: parser-micro alloc SHAPE ENGINE"

benchAll :: IO ()
benchAll = do
  mapM_ conserve shapes
  putStrLn "conservation: exact (all engines agree on every shape)"
  printf "%-14s %-9s %12s %10s\n" "shape" "engine" "us" "ns/B"
  forM_ shapes $ \(label, src) ->
    forM_ engines $ \(eng, f) -> do
      (ns, _) <- ticksN reps (length . f) src
      printf
        "%-14s %-9s %12.2f %10.2f\n"
        label
        eng
        (fromIntegral ns / 1000 :: Double)
        (fromIntegral ns / fromIntegral (BS.length src) :: Double)

-- | Every engine must produce the identical token stream; the run is
-- meaningless otherwise.
conserve :: (String, ByteString) -> IO ()
conserve (label, src) = case engines of
  [] -> pure ()
  ((e0, f0) : rest) ->
    mapM_
      (\(e, f) -> if f src == r0 then pure () else error ("conservation broken on " <> label <> ": " <> e <> " vs " <> e0))
      rest
    where
      r0 = f0 src
