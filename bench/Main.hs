{-# LANGUAGE OverloadedStrings #-}

-- | Criterion harness: Circuit.Markup vs markup-parse, over the calibrated
-- corpus plus a real chart-svg SVG artifact. Companion to the circuits-meter
-- harness ('Meter.hs') — both measure the identical closures from 'Workload'
-- over the identical inputs, so the two methods can be cross-checked.
module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Criterion.Main
import Data.ByteString qualified as B

import Circuit.Markup qualified as CM
import Corpus (ladder)
import MarkupParse qualified as MP
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
            [ bench "circuit-markup" $ whnf (cmTokens CM.Html) bs
            , bench "markup-parse" $ whnf (mpTokens MP.Html) bs
            ]
        , bgroup
            "markup"
            [ bench "circuit-markup" $ whnf (cmMarkupNodes CM.Html) bs
            , bench "markup-parse" $ whnf (mpMarkupNodes MP.Html) bs
            ]
        ]
    | (nm, bs) <- inputs
    ]
