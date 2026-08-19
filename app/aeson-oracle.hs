{-# LANGUAGE OverloadedStrings #-}

-- | circuits-parser aeson/cassava oracle ⟜ outward verification against
-- competitor libraries.
--
-- This executable was split out of @circuits-parser-axioma@ so that the core
-- @circuits-parser@ library does not depend on @aeson@ or @cassava@.  It runs:
--
--   * the JSONTestSuite corpus (shipped with aeson) as accept/reject oracle,
--     with a value-level differential against aeson itself;
--   * a CSV differential against cassava.
module Main (main) where

import Circuit.Parser.Csv (Csv (..), decodeCsv)
import Circuit.Parser.Json
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Csv qualified as Cassava
import Data.List (isSuffixOf, sortOn)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Vector qualified as V
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  corpusFails <- corpus
  let fails = corpusFails ++ csvDiff ++ csvDivergences
  if null fails
    then putStrLn "aeson-oracle: all green"
    else do
      mapM_ (hPutStrLn stderr) fails
      hPutStrLn stderr ("aeson-oracle: " ++ show (length fails) ++ " failure(s)")
      exitFailure

-- * JSONTestSuite corpus

testParsingDir :: FilePath
testParsingDir = "/Users/tonyday567/other/aeson/tests/JSONTestSuite/test_parsing"

-- | Files we deliberately do not run: UTF-16-encoded accepts (this package
-- is byte/UTF-8 only) and the 100k-deep nesting rejects (combinator
-- recursion depth is not the point of this oracle).
skipped :: FilePath -> Bool
skipped f =
  "utf16" `isIn` f
    || "100000" `isIn` f
    || "open_array_object" `isIn` f
  where
    isIn needle hay = any (needle `T.isPrefixOf`) (T.tails (T.pack hay))

corpus :: IO [String]
corpus = do
  there <- doesDirectoryExist testParsingDir
  if not there
    then do
      putStrLn "aeson-oracle: JSONTestSuite corpus not found, skipping"
      pure []
    else do
      files <- listDirectory testParsingDir
      let jsons = [f | f <- files, ".json" `isSuffixOf` f, not (skipped f)]
      concat <$> mapM runOne jsons

runOne :: FilePath -> IO [String]
runOne f = do
  s <- B.readFile (testParsingDir ++ "/" ++ f)
  let ours = decodeJson s
      aesons = A.decode (BL.fromStrict s) :: Maybe A.Value
  pure $ case take 2 f of
    "y_" -> case (ours, aesons) of
      (Right j, Just v)
        | normalize j == normalize (fromAeson v) -> []
        | otherwise -> [f ++ ": value mismatch vs aeson"]
      (Left e, _) -> [f ++ ": should parse, we said " ++ e]
      (_, Nothing) -> [f ++ ": aeson rejected, we accepted"]
    "n_" -> case ours of
      Left _ -> []
      Right _ -> [f ++ ": should reject, we accepted"]
    _ -> [] -- i_ files are implementation-defined

-- | Canonical form for the differential: aeson dedups object keys (first
-- wins) and its KeyMap lists keys sorted; we preserve source order and
-- duplicates on purpose. Sort, dedup-first, recurse.
normalize :: Json -> Json
normalize (JObject kvs) = JObject (sortOn fst (dedup [(k, normalize v) | (k, v) <- kvs]))
  where
    dedup = foldr (\(k, v) acc -> (k, v) : [(k', v') | (k', v') <- acc, k' /= k]) []
normalize (JArray a) = JArray (V.map normalize a)
normalize j = j

fromAeson :: A.Value -> Json
fromAeson (A.Object o) = JObject [(Key.toText k, fromAeson v) | (k, v) <- KM.toList o]
fromAeson (A.Array a) = JArray (V.map fromAeson a)
fromAeson (A.String t) = JString t
fromAeson (A.Number n) = JNumber n
fromAeson (A.Bool b) = JBool b
fromAeson A.Null = JNull

-- * csv

csv :: [[T.Text]] -> Either String Csv
csv = Right . Csv . V.fromList . map V.fromList

-- | Differential vs cassava: both sides must agree, accept and reject.
csvDiff :: [String]
csvDiff = concatMap go diffInputs
  where
    go (name, s) = case (decodeCsv s, cassava s) of
      (Right c, Right v) -> check ("csv-diff " ++ name) (toRows c) v
      (Left _, Left _) -> []
      (ours, theirs) -> ["csv-diff " ++ name ++ ": accept mismatch (ours " ++ side ours ++ ", cassava " ++ side theirs ++ ")"]
    cassava s = Cassava.decode Cassava.NoHeader (BL.fromStrict s) :: Either String (V.Vector (V.Vector B.ByteString))
    toRows (Csv v) = V.map (V.map encodeUtf8) v
    side = either (const "reject") (const "accept")
    diffInputs =
      [ ("basic", "a,b\r\nc,d\r\n"),
        ("lf endings", "a,b\nc,d\n"),
        ("no trailing eol", "a,b\nc,d"),
        ("quoted comma", "\"a,b\",c"),
        ("doubled quotes", "\"say \"\"hi\"\"\",ok"),
        ("embedded newline", "\"a\nb\",c"),
        ("empty fields", "a,,b"),
        ("trailing comma", "a,b,\n"),
        ("single column", "a\nb\nc"),
        ("empty input", ""),
        ("reject bare quote", "a\"b,c"),
        ("reject garbage after quote", "\"a\"x,b")
      ]

-- | Documented dialect divergences: cases where we and cassava deliberately
-- read the same bytes differently. Both sides are asserted exactly — a
-- divergence is a choice, and the choice is pinned here.
csvDivergences :: [String]
csvDivergences =
  concat
    [ -- blank lines: we keep them as records of one empty field; cassava drops them
      check "divergence blank line (ours)" (csv [["a"], [""], ["b"]]) (decodeCsv "a\n\nb"),
      check "divergence blank line (cassava)" (Right (V.fromList [V.fromList ["a"], V.fromList ["b"]])) (Cassava.decode Cassava.NoHeader ("a\n\nb" :: BL.ByteString) :: Either String (V.Vector (V.Vector B.ByteString))),
      -- unterminated quote: we reject; cassava lets EOF close the quote
      check "divergence unterminated (ours)" (Left "invalid CSV") (decodeCsv "a,\"b"),
      check "divergence unterminated (cassava)" (Right (V.fromList [V.fromList ["a", ""]])) (Cassava.decode Cassava.NoHeader ("a,\"b" :: BL.ByteString) :: Either String (V.Vector (V.Vector B.ByteString)))
    ]

check :: (Eq a, Show a) => String -> a -> a -> [String]
check name expected actual
  | expected == actual = []
  | otherwise = [name ++ ": expected " ++ show expected ++ ", got " ++ show actual]
