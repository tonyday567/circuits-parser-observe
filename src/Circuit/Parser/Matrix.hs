{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Small, value-sized square-matrix helpers for semiring closure.
--
-- This module replaces the previous dependency on @Circuit.Mat.Dense@.
-- It intentionally uses plain lists rather than typed arrays, because the
-- matrices here are indexed by runtime parser states and are dense and small.
module Circuit.Parser.Matrix
  ( Matrix,
    fromLists,
    toLists,
    starMatrix,
  )
where

import NumHask.Prelude hiding (sum)

-- | Square matrix stored as nested rows.
newtype Matrix a = Matrix {unMatrix :: [[a]]}
  deriving (Eq, Show)

-- | Build a matrix from nested rows.
--
-- An empty list becomes a 0×0 matrix.
fromLists :: [[a]] -> Matrix a
fromLists [] = Matrix []
fromLists xss = Matrix xss

-- | Convert a matrix to nested rows.
toLists :: Matrix a -> [[a]]
toLists = unMatrix

-- | Kleene star of a square matrix by the standard state-elimination
-- (Warshall / Floyd-Kleene) algorithm.
--
-- For a matrix @A@, computes @A* = I + A + A² + ...@ as the least fixed
-- point of @X ↦ I + A·X@.
starMatrix ::
  (StarSemiring a, Additive a, Multiplicative a) =>
  Matrix a ->
  Matrix a
starMatrix (Matrix []) = Matrix []
starMatrix (Matrix xss) =
  let n = length xss
      step arr k =
        [ [ let mkk = arr !! k !! k
                mik = arr !! i !! k
                mkj = arr !! k !! j
             in arr !! i !! j + mik * star mkk * mkj
            | j <- [0 .. n - 1]
          ]
        | i <- [0 .. n - 1]
        ]
      closed = foldl' step xss [0 .. n - 1]
      eye = [[bool zero one (i == j) | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
   in Matrix (zipWith (zipWith (+)) closed eye)
