{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | jsonl-micro: 'runJsonLexerBS' vs 'runJsonMachineBS'.
--
--   runJsonLexerBS  — the library lexer from Circuit.Parser.Json.Lexer:
--                     token-recursion with inner span loops, strict
--                     accumulator, literal validation, error offsets.
--   runJsonMachineBS — the same tokenizer as a Circuit.Machine Cell/Step:
--                     one tick per byte, explicit (Ctx, start) state,
--                     span-carrying emissions.
--   walk-only       — count-only traversal diagnostic: no slices, no
--                     tokens, no list. Reference for materialization cost.
--
-- The machine and the walk-only diagnostic live in bench/Engines.hs
-- (shared with wires-micro); the INLINE pragmas there are load-bearing
-- (loom/perf.md §the INLINE trap).
--
-- The lexer was restructured after a decomposition run of this suite showed
-- its Either fmap consing cost ~4 ns/token and literal validation ~0.6;
-- the strict-accumulator version keeps the exact error contract.
--
-- Corpus: a JSON value always tokenizes to an odd count, so round token
-- totals come from an A/B line pair (51 + 49 = 100 tokens); shapes are
-- exact multiples of 1000 so scaling is visible per-token.
--
-- Usage: jsonl-micro                    — conservation, then timing table
--        jsonl-micro alloc ENGINE TOKS  — single run (for +RTS -s)
module Main (main) where

import Circuit.Meter.Time (Nanos, nanos)
import Circuit.Parser.Json (JsonToken (..), runJsonLexerBS)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Engines (lexCount, runJsonMachineBS, runJsonMachineIO)
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

-- * harness

engines :: [(String, ByteString -> IO [JsonToken])]
engines =
  [ ("runJsonLexerBS", pure . either (error "runJsonLexerBS failed") id . runJsonLexerBS),
    ("runJsonMachineBS", pure . runJsonMachineBS),
    ("runJsonMachineIO", runJsonMachineIO)
  ]

scans :: [(String, ByteString -> IO Int)]
scans = [("walk-only", pure . lexCount)]

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
        (Just f, Nothing) -> hPrint stderr (BS.length src) >> (f src >>= print . length)
        (Nothing, Just f) -> hPrint stderr (BS.length src) >> (f src >>= print)
        _ -> error "usage: jsonl-micro alloc ENGINE TOKENS"

-- | Reps per shape, scaled so each measurement is ~30ms minimum.
repsFor :: Int -> Int
repsFor toks = max 30 (200000 `div` (toks `div` 10))

-- | Minimum (pure CPU, GC-lightest rep) and mean (GC amortised in) of
-- per-rep timings of an IO action, no built-in warmup.
statsOf :: Int -> IO a -> IO (Nanos, Nanos)
statsOf n act = go n []
  where
    go 0 ts = pure (minimum ts, sum ts `div` fromIntegral n)
    go i ts = do
      t0 <- nanos
      !_ <- act
      t1 <- nanos
      go (i - 1) ((t1 - t0) : ts)

-- | Force the full token spine inside the timed region.
timed :: (ByteString -> IO [JsonToken]) -> ByteString -> IO Int
timed f src = do
  ts <- f src
  let !l = length ts
  pure l

benchAll :: IO ()
benchAll = do
  mapM_ conserve shapes
  putStrLn "conservation: exact (all token engines agree, scans count the same)"
  warmup
  putStrLn "shape            bytes engine           ns/tok-min  ns/tok-mean    ns/B-min"
  forM_ shapes $ \(label, toks, src) -> do
    let row eng (nsMin, nsMean) =
          printf
            "%-14s %5d %-16s %10.3f %10.3f %10.3f\n"
            label
            (BS.length src)
            eng
            (fromIntegral nsMin / fromIntegral toks :: Double)
            (fromIntegral nsMean / fromIntegral toks :: Double)
            (fromIntegral nsMin / fromIntegral (BS.length src) :: Double)
    forM_ engines $ \(eng, f) -> statsOf (repsFor toks) (timed f src) >>= row eng
    forM_ scans $ \(eng, f) -> statsOf (repsFor toks) (f src) >>= row eng

warmup :: IO ()
warmup =
  forM_ shapes $ \(_, _, src) -> do
    forM_ engines $ \(_, f) -> statsOf 3 (timed f src)
    forM_ scans $ \(_, f) -> statsOf 3 (f src)

conserve :: (String, Int, ByteString) -> IO ()
conserve (label, toks, src) = case engines of
  [] -> pure ()
  ((e0, f0) : rest) -> do
    r0 <- f0 src
    if length r0 /= toks
      then error ("corpus off on " <> label <> ": expected " <> show toks <> " tokens, got " <> show (length r0))
      else
        forM_
          rest
          ( \(e, f) -> do
              r <- f src
              if r == r0 then pure () else error ("conservation broken on " <> label <> ": " <> e <> " vs " <> e0)
          )
    forM_
      scans
      ( \(e, f) -> do
          n <- f src
          if n == toks then pure () else error ("scan count off on " <> label <> ": " <> e)
      )
