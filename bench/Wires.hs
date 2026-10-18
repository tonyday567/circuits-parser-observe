{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | wires-micro: the jsonl engines meet real wire transcripts.
--
--   wires-micro store <files...>   — the store-wide pass
--   wires-micro scale <wire> [MB,MB,...] — byte-budget ladder, one wire
--
-- store: walk-only / runJsonLexerBS / runJsonMachineBS over every line of
-- every file (3 reps, min+mean); decodeJson (whnf, the wordcount cost)
-- and decodeJson + node count (full tree materialization) over the first
-- 25 files (1 rep — the store-wide decode rate was banked in wave 0:
-- 795.5 s, coffee/loom/transcripts-perf.md).
--
-- scale: per-arm ns/B at each budget; the parenthesised figure is the
-- time ratio vs the previous size — tracking the byte ratio means linear.
--
-- Conservation: lexer and machine token totals must agree exactly;
-- decode must succeed on every line (wave 0: parse fails 0 store-wide).
module Main (main) where

import Circuit.Meter.Time (Nanos, nanos)
import Circuit.Parser.Json (Json (..), decodeJson, runJsonLexerBS)
import Control.Exception (evaluate)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Foldable (foldl')
import Data.Vector qualified as V
import Engines (lexCount, runJsonMachineBS)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("store" : files@(_ : _)) -> storeMain False files
    ("storeall" : files@(_ : _)) -> storeMain True files
    ("scale" : f : s : _) -> scaleMain f (parseSizes s)
    ("scale" : f : _) -> scaleMain f [1, 2, 4, 8, 16, 24]
    _ -> error "usage: wires-micro store <files...> | wires-micro storeall <files...> | wires-micro scale <wire> [sizes]"
  where
    parseSizes = map read . words . map (\c -> if c == ',' then ' ' else c)

-- * per-line engines (count-producing; the count forces the work)

walkOnlyE, lexerE, machineE, decodeE, decodeNFE :: ByteString -> Int
walkOnlyE = lexCount
lexerE = either (error "runJsonLexerBS failed") length . runJsonLexerBS
machineE = length . runJsonMachineBS
decodeE l = case decodeJson l of
  Right _ -> 1
  Left _ -> 0
decodeNFE l = case decodeJson l of
  Right j -> nodes j
  Left _ -> 0

-- | Tree node count, strict — forces full materialization of the Json.
nodes :: Json -> Int
nodes = \case
  JArray v -> 1 + V.foldl' (\a j -> a + nodes j) 0 v
  JObject ps -> 1 + foldl' (\a (_, j) -> a + nodes j) 0 ps
  _ -> 1

arms :: [(String, ByteString -> Int)]
arms =
  [ ("walk-only", walkOnlyE),
    ("lexer", lexerE),
    ("machine", machineE),
    ("decode", decodeE),
    ("decodeNF", decodeNFE)
  ]

fastArms, slowArms :: [(String, ByteString -> Int)]
(fastArms, slowArms) = splitAt 3 arms

-- | Fold an engine over lines; the strict sum forces each line's count
-- (and with it the line's token spine or tree) inside the fold.
runLines :: (ByteString -> Int) -> [ByteString] -> IO Int
runLines f ls = evaluate (foldl' (\a l -> a + f l) 0 ls)

-- * harness: min + mean of n reps, no built-in warmup

statsOf :: Int -> IO a -> IO (Nanos, Nanos)
statsOf n act = go n []
  where
    go 0 ts = pure (minimum ts, sum ts `div` fromIntegral n)
    go i ts = do
      t0 <- nanos
      !_ <- act
      t1 <- nanos
      go (i - 1) ((t1 - t0) : ts)

ms :: Nanos -> String
ms n = take 8 (show (fromIntegral n / 1e6 :: Double))

perb :: Nanos -> Int -> Double
perb ns b = fromIntegral ns / fromIntegral b :: Double

-- * store mode

-- | decodeAll = run the slow arms over every file (they default to the
-- first 25, since a store-wide decode rep is minutes).
storeMain :: Bool -> [FilePath] -> IO ()
storeMain decodeAll files = do
  t0 <- nanos
  contents <- mapM BS.readFile files
  bytes <- evaluate (sum (map BS.length contents))
  t1 <- nanos
  let fileLines = map C8.lines contents
      ls = concat fileLines
      slowLs = if decodeAll then ls else concat (take 25 fileLines)
      slowB = sum (map BS.length slowLs)
  _ <- evaluate (length ls)
  putStrLn ("files: " <> show (length files) <> ", bytes: " <> show bytes <> ", lines: " <> show (length ls))
  putStrLn ("io floor (read): " <> ms (t1 - t0) <> " ms")

  toks <- conserve ls
  printf "tokens: %d (%.3f B/tok)\n" toks (fromIntegral bytes / fromIntegral toks :: Double)
  mapM_ (fastRow ls bytes toks) fastArms
  putStrLn ("decode arms over " <> (if decodeAll then "ALL files" else "first 25 files") <> " (" <> show slowB <> " B, 1 rep):")
  mapM_ (slowRow slowLs slowB) slowArms
  where
    fastRow ls bytes toks (eng, f) = do
      (nsMin, nsMean) <- statsOf 3 (runLines f ls)
      printf
        "%-10s %10.3f ns/tok-min %10.3f ns/tok-mean %10.3f ns/B-min\n"
        eng
        (fromIntegral nsMin / fromIntegral toks :: Double)
        (fromIntegral nsMean / fromIntegral toks :: Double)
        (perb nsMin bytes)
    slowRow slowLs slowB (eng, f) = do
      (nsMin, nsMean) <- statsOf 1 (runLines f slowLs)
      printf "%-10s %10.1f ns/B-min %10.1f ns/B-mean\n" eng (perb nsMin slowB) (perb nsMean slowB)

-- | Lexer and machine must produce identical token totals.
conserve :: [ByteString] -> IO Int
conserve ls = do
  tl <- runLines lexerE ls
  tm <- runLines machineE ls
  if tl == tm
    then putStrLn "conservation: exact (lexer == machine token totals)" >> pure tl
    else error ("CONSERVATION BROKEN: lexer=" <> show tl <> " machine=" <> show tm)

-- * scale mode

scaleMain :: FilePath -> [Double] -> IO ()
scaleMain f sizes = do
  ls <- C8.lines <$> BS.readFile f
  putStrLn ("wire: " <> f <> ", " <> show (length ls) <> " lines")
  results <- mapM (measure ls) sizes
  mapM_ (putStrLn . uncurry reportRow) (zip (Nothing : map Just results) results)
  where
    measure ls mb = do
      let chunk = takeBudget (floor (mb * 1024 * 1024)) ls
      cb <- evaluate (sum (map BS.length chunk))
      timings <- mapM (\(_, e) -> fst <$> statsOf 1 (runLines e chunk)) arms
      pure (mb, cb, timings)

reportRow :: Maybe (Double, Int, [Nanos]) -> (Double, Int, [Nanos]) -> String
reportRow prev (mb, cb, timings) =
  printf "%6.2fMB %9d B: " mb cb
    <> unwords (zipWith cell arms timings)
  where
    cell (eng, _) t =
      printf "%s %.1f%s" eng (perb t cb) (rate prevTs)
      where
        prevTs = case prev of
          Nothing -> Nothing
          Just (_, _, pts) -> lookup eng (zip (map fst arms) pts)
        rate :: Maybe Nanos -> String
        rate Nothing = ""
        rate (Just pt) = printf " (x%.2f)" (perb t cb / perb pt cb)

-- | The longest prefix of lines whose cumulative size stays within the
-- byte budget.
takeBudget :: Int -> [ByteString] -> [ByteString]
takeBudget budget = go 0
  where
    go !_ [] = []
    go !acc (l : rest)
      | acc + BS.length l > budget = []
      | otherwise = l : go (acc + BS.length l) rest
