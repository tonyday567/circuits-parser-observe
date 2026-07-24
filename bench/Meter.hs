-- | circuits-meter harness: the same Workload closures over the same Corpus
-- inputs as the criterion harness ('Main.hs'), measured with
-- 'Circuit.Meter.Time.ticksN' instead of criterion.
module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.ByteString qualified as B
import Text.Printf (printf)

import Circuit.Meter.Time (ticksN)
import Corpus (ladder)
import MarkupParse qualified as MP
import MarkupParseLegacy qualified as MPL
import Workload

iterations :: Int
iterations = 100

loadInputs :: IO [(String, B.ByteString)]
loadInputs = do
  svg <- B.readFile "corpus/chart-wheel.svg"
  mapM (\(nm, bs) -> do b <- evaluate (force bs); pure (nm, b)) (ladder <> [("chart-wheel-svg-141KB", svg)])

-- | Time one pure function over one input, n iterations, report mean µs.
timeUs :: Int -> (a -> Int) -> a -> IO Double
timeUs n f a = do
  (meanNanos, _) <- ticksN n f a
  pure (fromIntegral meanNanos / 1000)

main :: IO ()
main = do
  inputs <- loadInputs
  printf "%-26s %-10s %12s %12s %10s\n" "input" "stage" "mp-0.3-µs" "mp-0.2.2.0-µs" "ratio" :: IO ()
  forM_ inputs $ \(nm, bs) -> do
    -- tokenize
    ct <- timeUs iterations (mpTokens MP.Html) bs
    mt <- timeUs iterations (mplTokens MPL.Html) bs
    printf "%-26s %-10s %12.2f %12.2f %9.2fx\n" nm "tokenize" ct mt (ct / mt) :: IO ()
    -- markup (full tree)
    cm <- timeUs iterations (mpMarkupNodes MP.Html) bs
    mm <- timeUs iterations (mplMarkupNodes MPL.Html) bs
    printf "%-26s %-10s %12.2f %12.2f %9.2fx\n" nm "markup" cm mm (cm / mm) :: IO ()
