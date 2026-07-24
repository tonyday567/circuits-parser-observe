{-# LANGUAGE OverloadedStrings #-}

-- | Shared workload definitions for both harnesses.
--
-- The point of a shared module is that the criterion harness and the
-- circuits-meter harness measure /byte-identical closures/ over /byte-identical
-- inputs/ — any divergence in results is then attributable to the measurement
-- method, not to what was measured.
--
-- == The 'These'/'That' decoding subtlety
--
-- Both parsers return @Warn a = These [warning] a@. Per their documented
-- behaviour, a fully-consumed parse can surface as 'That' carrying the result,
-- 'This' (clean), or 'These' (result + warnings). Earlier measurement treated
-- 'That' as failure and returned a sentinel, which both under-forced the result
-- and mislabelled a successful parse. Here we force the real payload in every
-- branch via a token count, so the timed work is the actual parse + full spine
-- walk.
module Workload
  ( mpTokens,
    mplTokens,
    mpMarkupNodes,
    mplMarkupNodes,
  )
where

import Data.ByteString (ByteString)
import Data.These (These (..))
import Data.Tree qualified as Tree
import MarkupParse qualified as MP
import MarkupParseLegacy qualified as MPL

-- | Force a 'Warn'-wrapped list to its length. @Warn a = These [warning] a@,
-- so the result payload lives in the 'That'/'These' right slot; 'This' carries
-- warnings only (no result) and counts as zero.
theseLen :: These w [a] -> Int
theseLen (This _) = 0
theseLen (That xs) = length xs
theseLen (These _ xs) = length xs

-- | tokenize + full spine walk, markup-parse 0.3 (circuits-parser).
mpTokens :: MP.Standard -> ByteString -> Int
mpTokens std = theseLen . MP.tokenize std

-- | tokenize + full spine walk, markup-parse 0.2.2.0 (flatparse).
mplTokens :: MPL.Standard -> ByteString -> Int
mplTokens std = theseLen . MPL.tokenize std

-- | Full parse to markup tree, then count nodes (forces the whole forest),
-- markup-parse 0.3.
mpMarkupNodes :: MP.Standard -> ByteString -> Int
mpMarkupNodes std bs = case MP.markup std bs of
  This _ -> 0
  That m -> nodes m
  These _ m -> nodes m
  where
    nodes = sum . map (length . Tree.flatten) . MP.elements

-- | Full parse to markup tree, then count nodes, markup-parse 0.2.2.0.
mplMarkupNodes :: MPL.Standard -> ByteString -> Int
mplMarkupNodes std bs = case MPL.markup std bs of
  This _ -> 0
  That m -> nodes m
  These _ m -> nodes m
  where
    nodes = sum . map (length . Tree.flatten) . MPL.elements
