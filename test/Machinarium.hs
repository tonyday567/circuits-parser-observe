{-# LANGUAGE OverloadedStrings #-}

-- | Machinarium executable oracle for markup-parse tokenization.
--
-- Like 'circuits-poly/test/Spec.hs' for axioms, this file is the spec. It
-- states the performance oracles, runs the measurements, and reports
-- PASS / FAIL. An agent given only this file can see what is being claimed
-- and how it is verified.
module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.These (These (..))
import Data.Tree qualified as Tree
import System.Exit (exitFailure)
import Text.Printf (printf)

import Circuit.Meter.Time (ticksN)
import MarkupParse qualified as MP
import MarkupParseLegacy qualified as MPL

-- ---------------------------------------------------------------------------
-- Test harness
-- ---------------------------------------------------------------------------

assert :: String -> Bool -> IO ()
assert msg ok =
  if ok
    then putStrLn ("  PASS " ++ msg)
    else do
      putStrLn ("  FAIL " ++ msg)
      exitFailure

-- ---------------------------------------------------------------------------
-- Workload: force the full parse result so the timed work is real.
-- ---------------------------------------------------------------------------

theseLen :: These w [a] -> Int
theseLen (This _) = 0
theseLen (That xs) = length xs
theseLen (These _ xs) = length xs

mpTokens :: MP.Standard -> ByteString -> Int
mpTokens std = theseLen . MP.tokenize std

mplTokens :: MPL.Standard -> ByteString -> Int
mplTokens std = theseLen . MPL.tokenize std

mpMarkupNodes :: MP.Standard -> ByteString -> Int
mpMarkupNodes std bs = case MP.markup std bs of
  This _ -> 0
  That m -> nodes m
  These _ m -> nodes m
  where
    nodes = sum . map (length . Tree.flatten) . MP.elements

mplMarkupNodes :: MPL.Standard -> ByteString -> Int
mplMarkupNodes std bs = case MPL.markup std bs of
  This _ -> 0
  That m -> nodes m
  These _ m -> nodes m
  where
    nodes = sum . map (length . Tree.flatten) . MPL.elements

-- ---------------------------------------------------------------------------
-- Measurement helpers
-- ---------------------------------------------------------------------------

timeUs :: Int -> (a -> Int) -> a -> IO Double
timeUs n f a = do
  (meanNanos, _) <- ticksN n f a
  pure (fromIntegral meanNanos / 1000)

-- ---------------------------------------------------------------------------
-- Phase 1: floor
-- ---------------------------------------------------------------------------

-- The theoretical floor for tokenizing 141 KB of markup is a single-pass byte
-- scan. At ~1 GB/s memory bandwidth that is <1 ms. We do not assert an exact
-- number; we assert the oracle (flatparse) is in the right order of magnitude.

phase1Floor :: IO ()
phase1Floor = do
  putStrLn "Phase 1: theoretical floor"
  putStrLn "  A single-pass scan of 141 KB should be <1 ms at ~1 GB/s."

-- ---------------------------------------------------------------------------
-- Phase 2: best-practice oracle
-- ---------------------------------------------------------------------------

-- markup-parse 0.2.2.0 uses flatparse. It is the best-practice oracle for
-- this workload.

phase2Oracle :: ByteString -> IO (Double, Double)
phase2Oracle bs = do
  putStrLn "Phase 2: best-practice oracle (markup-parse 0.2.2.0 / flatparse)"
  ot <- timeUs 100 (mplTokens MPL.Html) bs
  om <- timeUs 100 (mplMarkupNodes MPL.Html) bs
  printf "  tokenize %.2f µs, markup %.2f µs\n" ot om
  assert "oracle tokenize is <5 ms (order-of-magnitude floor)" (ot < 5000)
  assert "oracle markup is <5 ms (order-of-magnitude floor)" (om < 5000)
  pure (ot, om)

-- ---------------------------------------------------------------------------
-- Phase 3: subject
-- ---------------------------------------------------------------------------

-- markup-parse 0.3 uses the circuits-parser engine. This is the substrate
-- implementation whose gap we are classifying.

phase3Subject :: ByteString -> IO (Double, Double)
phase3Subject bs = do
  putStrLn "Phase 3: subject (markup-parse 0.3 / circuits-parser)"
  st <- timeUs 10 (mpTokens MP.Html) bs
  sm <- timeUs 10 (mpMarkupNodes MP.Html) bs
  printf "  tokenize %.2f µs, markup %.2f µs\n" st sm
  pure (st, sm)

-- ---------------------------------------------------------------------------
-- Phase 4: diagnosis
-- ---------------------------------------------------------------------------

-- The profile currently classifies the gap as code residue: Identity bind,
-- pure, and withObDict dominate the profile. The executable checks that the
-- measured ratios are consistent with that classification (large but not
-- asymptotic).

phase4Diagnosis :: (Double, Double) -> (Double, Double) -> IO ()
phase4Diagnosis (ot, om) (st, sm) = do
  putStrLn "Phase 4: gap diagnosis"
  let rt = st / ot
      rm = sm / om
  printf "  tokenize ratio %.1fx, markup ratio %.1fx\n" rt rm
  assert "tokenize ratio is <500x (not absurdly beyond engine residue)" (rt < 500)
  assert "markup ratio is <500x (not absurdly beyond engine residue)" (rm < 500)
  assert "tokenize ratio is >10x (there is a real gap to explain)" (rt > 10)
  assert "markup ratio is >10x (there is a real gap to explain)" (rm > 10)
  putStrLn "  Classification: code residue (large constant-factor slowdown)."
  putStrLn "  Next step: dump Core for Data.Markup.Parser.tokenHtmlP and check"
  putStrLn "  why Identity bind / withObDict survive compilation."

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "markup-parse tokenization machinarium"
  bs <- B.readFile "corpus/chart-wheel.svg"
  !bs' <- evaluate (force bs)
  phase1Floor
  oracle <- phase2Oracle bs'
  subject <- phase3Subject bs'
  phase4Diagnosis oracle subject
  putStrLn "All machinarium oracles passed."
