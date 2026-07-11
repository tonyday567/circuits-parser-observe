{-# LANGUAGE OverloadedStrings #-}

-- | Calibrated markup corpus generator.
--
-- Produces valid, well-formed XML documents at controlled size knobs so the
-- two markup parsers can be measured on inputs with known structure, rather
-- than only on real files. Generic across consumers — the shapes here are
-- markup-parser stressors (nesting depth, sibling breadth, attribute count,
-- content length), not chart-specific.
--
-- All generators are deterministic (no randomness) so runs are reproducible
-- and both harnesses see byte-identical input.
module Corpus
  ( -- * Calibrated generators
    flat,
    nested,
    wideAttrs,
    contentHeavy,

    -- * A labelled size ladder
    ladder,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C

-- | @flat n@ — one root with @n@ empty sibling elements. Stresses the
-- sibling/tokenize loop with minimal tree depth.
--
-- >>> flat 2
-- "<root><i/><i/></root>"
flat :: Int -> ByteString
flat n = "<root>" <> C.concat (replicate n "<i/>") <> "</root>"

-- | @nested d@ — a single chain nested @d@ deep. Stresses the gather/tree
-- builder's depth handling (open-stack growth).
--
-- >>> nested 3
-- "<a><a><a></a></a></a>"
nested :: Int -> ByteString
nested d = C.concat (replicate d "<a>") <> C.concat (replicate d "</a>")

-- | @wideAttrs n k@ — @n@ siblings each carrying @k@ attributes. Stresses
-- the attribute parser specifically (the part real SVG/HTML leans on hardest).
wideAttrs :: Int -> Int -> ByteString
wideAttrs n k =
  "<root>"
    <> C.concat (replicate n ("<e " <> attrs <> "/>"))
    <> "</root>"
  where
    attrs = C.unwords [C.pack ("a" <> show i <> "=\"v" <> show i <> "\"") | i <- [1 .. k]]

-- | @contentHeavy n len@ — @n@ elements each wrapping a text block of @len@
-- characters. Stresses content scanning and escape handling.
contentHeavy :: Int -> Int -> ByteString
contentHeavy n len =
  "<root>"
    <> C.concat (replicate n ("<p>" <> C.replicate len 'x' <> "</p>"))
    <> "</root>"

-- | A labelled size ladder covering the four shapes at small/mid/large scales.
-- Each entry: (label, document). Byte sizes are reported by the harness.
ladder :: [(String, ByteString)]
ladder =
  [ ("flat-100", flat 100)
  , ("flat-2000", flat 2000)
  , ("nested-500", nested 500)
  , ("wideAttrs-500x8", wideAttrs 500 8)
  , ("contentHeavy-500x40", contentHeavy 500 40)
  ]
