-- | Machine observation for the circuits-parser substrate.
--
-- This is the machinarium treatment for @circuits-parser@: inward study
-- of how the parser engine behaves on the machine. It is not a benchmark
-- against other libraries; it measures the engine itself so gaps can be
-- named, classified, and followed over time.
module Main where

import Circuit.Parser qualified as CP
import Circuit.Meter.Time (ticksN)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import Data.Char (isAlphaNum)
import Data.Functor.Identity (Identity, runIdentity)

-- | Synthetic 1 MB input of words and punctuation.
tokenizerInput :: ByteString
tokenizerInput = C.pack (concat (replicate 17000 chunk))
  where
    chunk = "The quick brown fox jumps over 13 lazy dogs. " ++ punctuation ++ " "
    punctuation = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

isTokenChar :: Char -> Bool
isTokenChar = isAlphaNum

isSeparator :: Char -> Bool
isSeparator = not . isTokenChar

-- | Word tokenizer using circuits-parser / Identity.
cpWord :: CP.Parser Identity ByteString Char ByteString
cpWord = CP.bs (CP.some (CP.satisfy isTokenChar))

cpTokenizer :: ByteString -> [ByteString]
cpTokenizer s = cpTokenizeLoop (CP.skipWhile isSeparator *> cpWord) s
  where
    cpTokenizeLoop p = go
      where
        go s' = case runIdentity (CP.runParser p s') of
          CP.That _ -> []
          CP.This a -> [a]
          CP.These a s'' -> a : go s''

-- | Measure the Identity tokenizer throughput.
observeTokenizer :: IO ()
observeTokenizer = do
  putStrLn "-- circuits-parser Identity tokenizer"
  (t, toks) <- ticksN 50 (length . cpTokenizer) tokenizerInput
  let mb = fromIntegral (C.length tokenizerInput) / (1024 * 1024) :: Double
      secs = fromIntegral t / 1e9 :: Double
  putStrLn $ "  1 MB tokenized in " ++ show t ++ " ns/iter, " ++ show toks ++ " tokens"
  putStrLn $ "  throughput: " ++ show (mb / secs) ++ " MB/s"

main :: IO ()
main = do
  putStrLn "circuits-parser-observe"
  observeTokenizer
