{-# LANGUAGE OverloadedStrings #-}

module Main where

import Circuit.Parser qualified as CPU
import Control.Applicative (many, (<|>))
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import Data.Char (isAlphaNum)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Maybe (catMaybes)
import Data.These (These (..))
import Data.Void (Void)
import FlatParse.Basic qualified as F
import Test.Tasty.Bench
import Text.Megaparsec qualified as M
import Text.Parsec qualified as P

-- ---------------------------------------------------------------------------
-- Inputs
-- ---------------------------------------------------------------------------

-- | A synthetic 1 MB stream of words, numbers, and punctuation.
tokenizerInput :: ByteString
tokenizerInput = C.pack (concat (replicate 17000 chunk))
  where
    chunk = "The quick brown fox jumps over 13 lazy dogs. " ++ punctuation ++ " "
    punctuation = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

-- | A smaller synthetic tokenizer input for effectful parsers that have
-- higher per-step overhead.
tokenizerInputSmall :: ByteString
tokenizerInputSmall = C.pack (concat (replicate 2500 chunk))
  where
    chunk = "The quick brown fox jumps over 13 lazy dogs. " ++ punctuation ++ " "
    punctuation = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

-- | A synthetic markup document (~140 KB) of nested tags and content.
markupInput :: ByteString
markupInput = C.pack (concat (replicate 3000 "<div class=\"foo\"><p>hello, world 123</p><br/></div>"))

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isTokenChar :: Char -> Bool
isTokenChar = isAlphaNum

isSeparator :: Char -> Bool
isSeparator = not . isTokenChar

-- | Repeatedly run a circuits-parser until the stream is exhausted.
cpTokenizeLoop :: CPU.Parser Identity f s a -> f -> [a]
cpTokenizeLoop p = go
  where
    go s = case CPU.runParserIdentity p s of
      That _ -> []
      This a -> [a]
      These a s' -> a : go s'

-- ---------------------------------------------------------------------------
-- Tokenizer parsers
-- ---------------------------------------------------------------------------

-- | Word tokenizer for circuits-parser (Identity instantiation).
cpWord :: CPU.Parser Identity ByteString Char ByteString
cpWord = CPU.bs (CPU.some (CPU.satisfy isTokenChar))

cpTokenizer :: ByteString -> [ByteString]
cpTokenizer = cpTokenizeLoop (CPU.skipWhile isSeparator *> cpWord)

-- | Word tokenizer for circuits-parser with megaparsec-style state + errors.
cpExceptTokenizer :: ByteString -> [ByteString]
cpExceptTokenizer = go
  where
    cpExceptWord :: CPU.Parser (ExceptT String Identity) ByteString Char ByteString
    cpExceptWord = CPU.bs (CPU.some (CPU.satisfy isTokenChar))
    p :: CPU.Parser (ExceptT String Identity) ByteString Char ByteString
    p = CPU.skipWhile isSeparator *> cpExceptWord
    go s = case runIdentity (runExceptT (CPU.runParser p s)) of
      Right (These x s') -> x : go s'
      Right (This x) -> [x]
      Right (That _) -> []
      Left _ -> []

-- | Word tokenizer for attoparsec.
attoparsecWord :: A.Parser ByteString
attoparsecWord = A.takeWhile1 isTokenChar

attoparsecTokenizer :: ByteString -> [ByteString]
attoparsecTokenizer =
  either error id
    . A.parseOnly
      ( many (A.skipWhile isSeparator *> attoparsecWord)
          <* A.skipWhile isSeparator
          <* A.endOfInput
      )

-- | Word tokenizer for flatparse.
flatparseWord :: F.ParserT st e ByteString
flatparseWord = F.byteStringOf (F.some (F.satisfy isTokenChar))

flatparseTokenizer :: ByteString -> [ByteString]
flatparseTokenizer bs = case F.runParser (go []) bs of
  F.OK ws _ -> reverse ws
  F.Fail -> error "flatparse tokenizer failed"
  F.Err _ -> error "flatparse tokenizer errored"
  where
    go ws = do
      F.skipMany (F.satisfy isSeparator)
      (ws <$ F.eof)
        F.<|> do
          w <- flatparseWord
          go (w : ws)

-- | Word tokenizer for megaparsec (over String).
type MegaParser = M.Parsec Void String

megaparsecWord :: MegaParser String
megaparsecWord = M.some (M.satisfy isTokenChar)

megaparsecTokenizer :: String -> [String]
megaparsecTokenizer =
  either (error . M.errorBundlePretty) id
    . M.parse
      (catMaybes <$> M.many tokenOrSkip <* M.eof)
      ""
  where
    tokenOrSkip = (Just <$> megaparsecWord) <|> (Nothing <$ M.satisfy isSeparator)

-- | Word tokenizer for parsec (over String).
type ParsecParser = P.Parsec String ()

parsecWord :: ParsecParser String
parsecWord = P.many1 (P.satisfy isTokenChar)

parsecTokenizer :: String -> [String]
parsecTokenizer =
  either (error . show) id
    . P.parse
      (catMaybes <$> P.many tokenOrSkip <* P.eof)
      ""
  where
    tokenOrSkip = (Just <$> parsecWord) P.<|> (Nothing <$ P.satisfy isSeparator)

-- ---------------------------------------------------------------------------
-- Markup tokenizers
-- ---------------------------------------------------------------------------

-- | A simple markup tokenizer for circuits-parser: tag names and content.
cpMarkupTokenizer :: ByteString -> [ByteString]
cpMarkupTokenizer = cpTokenizeLoop tok
  where
    tagOpen = CPU.string "<" *> CPU.bs (CPU.some (CPU.satisfy (/= '>')))
    content = CPU.bs (CPU.some (CPU.satisfy (/= '<')))
    tok = tagOpen CPU.<|> content

-- | A simple markup tokenizer for circuits-parser with error effects.
cpExceptMarkupTokenizer :: ByteString -> [ByteString]
cpExceptMarkupTokenizer = go
  where
    tagOpen = CPU.string "<" *> CPU.bs (CPU.some (CPU.satisfy (/= '>')))
    content = CPU.bs (CPU.some (CPU.satisfy (/= '<')))
    tok :: CPU.Parser (ExceptT String Identity) ByteString Char ByteString
    tok = tagOpen CPU.<|> content
    go s = case runIdentity (runExceptT (CPU.runParser tok s)) of
      Right (These x s') -> x : go s'
      Right (This x) -> [x]
      Right (That _) -> []
      Left _ -> []

-- | A simple markup tokenizer for attoparsec.
attoparsecMarkupTokenizer :: ByteString -> [ByteString]
attoparsecMarkupTokenizer =
  either error id
    . A.parseOnly (many (tagOpen <|> content) <* A.endOfInput)
  where
    tagOpen = A.string "<" *> A.takeWhile (/= '>')
    content = A.takeWhile1 (/= '<')

-- | A simple markup tokenizer for flatparse.
flatparseMarkupTokenizer :: ByteString -> [ByteString]
flatparseMarkupTokenizer bs = case F.runParser (go []) bs of
  F.OK ws _ -> reverse ws
  F.Fail -> error "flatparse markup tokenizer failed"
  F.Err _ -> error "flatparse markup tokenizer errored"
  where
    go ws =
      (ws <$ F.eof)
        F.<|> do
          w <- (F.satisfy (== '<') *> F.byteStringOf (F.some (F.satisfy (/= '>')))) F.<|> F.byteStringOf (F.some (F.satisfy (/= '<')))
          go (w : ws)

-- ---------------------------------------------------------------------------
-- Benchmarks
-- ---------------------------------------------------------------------------

tokenizerBenchmarks :: Benchmark
tokenizerBenchmarks =
  bgroup
    "tokenizer"
    [ bench "circuits-parser/Identity" $ nf cpTokenizer tokenizerInput,
      bench "circuits-parser/Except" $ nf cpExceptTokenizer tokenizerInputSmall,
      bench "attoparsec" $ nf attoparsecTokenizer tokenizerInput,
      bench "flatparse" $ nf flatparseTokenizer tokenizerInput,
      bench "megaparsec" $ nf megaparsecTokenizer (C.unpack tokenizerInput),
      bench "parsec" $ nf parsecTokenizer (C.unpack tokenizerInput)
    ]

markupBenchmarks :: Benchmark
markupBenchmarks =
  bgroup
    "markup-tokenize"
    [ bench "circuits-parser/Identity" $ nf cpMarkupTokenizer markupInput,
      bench "circuits-parser/Except" $ nf cpExceptMarkupTokenizer markupInput,
      bench "attoparsec" $ nf attoparsecMarkupTokenizer markupInput,
      bench "flatparse" $ nf flatparseMarkupTokenizer markupInput
    ]

main :: IO ()
main = defaultMain [tokenizerBenchmarks, markupBenchmarks]
