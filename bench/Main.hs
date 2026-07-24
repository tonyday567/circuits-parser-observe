{-# LANGUAGE OverloadedStrings #-}

-- | Criterion harness: markup-parse 0.3 vs markup-parse 0.2.2.0 (flatparse),
-- over the calibrated corpus plus a real chart-svg SVG artifact. Companion to
-- the circuits-meter harness ('Meter.hs') — both measure the identical closures
-- from 'Workload' over the identical inputs.
module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Criterion.Main
import Data.ByteString qualified as B

import Corpus (ladder)
import MarkupParse qualified as MP
import MarkupParseLegacy qualified as MPL
import Workload

-- | Inputs: calibrated corpus (Xml) + a real chart-svg document.
loadInputs :: IO [(String, B.ByteString)]
loadInputs = do
  svg <- B.readFile "corpus/chart-wheel.svg"
  let synthetic = ladder
  mapM (\(nm, bs) -> do b <- evaluate (force bs); pure (nm, b)) (synthetic <> [("chart-wheel-svg-141KB", svg)])

main :: IO ()
main = do
  inputs <- loadInputs
  defaultMain
    [ bgroup
        nm
        [ bgroup
            "tokenize"
            [ bench "markup-parse-0.3" $ whnf (mpTokens MP.Html) bs,
              bench "markup-parse-0.2.2.0" $ whnf (mplTokens MPL.Html) bs
            ]
        , bgroup
            "markup"
            [ bench "markup-parse-0.3" $ whnf (mpMarkupNodes MP.Html) bs,
              bench "markup-parse-0.2.2.0" $ whnf (mplMarkupNodes MPL.Html) bs
            ]
        ]
    | (nm, bs) <- inputs
    ]
