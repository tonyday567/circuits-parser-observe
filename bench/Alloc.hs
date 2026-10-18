{-# LANGUAGE OverloadedStrings #-}

-- | Per-shape allocation probe: one Workload closure, one input, one run.
--
-- The meter harness aggregates all shapes into a single RTS allocation
-- figure; this probe exists to attribute it. Run with +RTS -s and divide
-- by the input size.
--
-- Usage: markup-alloc tokenize|markup flat-100|flat-2000|nested-500|wideAttrs-500x8|contentHeavy-500x40|svg
module Main (main) where

import Corpus (ladder)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import MarkupParse qualified as MP
import System.Environment (getArgs)
import System.IO (hPrint, stderr)
import Workload (mpMarkupNodes, mpTokens)

main :: IO ()
main = do
  [stage, label] <- getArgs
  bs <- case label of
    "svg" -> B.readFile "corpus/chart-wheel.svg"
    -- micro-shapes: per-construct cost bisection
    "c0" -> pure ""
    "c1" -> pure "x"
    "c40" -> pure (C.replicate 40 'x')
    "c400" -> pure (C.replicate 400 'x')
    "lt" -> pure "<"
    "ltname" -> pure "<r"
    "open" -> pure "<r>"
    "end" -> pure "</r>"
    "openattr" -> pure "<r a=\"v\">"
    "openws" -> pure "<r  >"
    "bool" -> pure "<r x/>"
    "empty" -> pure "<r/>"
    "pair" -> pure "<r></r>"
    "attr1" -> pure "<r a=\"v\"/>"
    "attr2" -> pure "<r a=\"v\" b=\"w\"/>"
    "comment" -> pure "<!--c-->"
    _ -> case lookup label ladder of
      Just b -> pure b
      Nothing -> error ("unknown shape: " <> label)
  let f = case stage of
        "tokenize" -> mpTokens MP.Html
        "markup" -> mpMarkupNodes MP.Html
        _ -> error ("unknown stage: " <> stage)
  hPrint stderr (B.length bs)
  print (f bs)
