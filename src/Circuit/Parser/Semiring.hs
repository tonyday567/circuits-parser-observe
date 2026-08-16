{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Carriers for semiring-weighted parsing, wired into the 'numhask'
-- hierarchy.
--
-- The semiring classes themselves live in 'NumHask.Algebra.Ring' and
-- 'NumHask.Algebra.Additive' / 'Multiplicative'.  This module supplies the
-- carriers that do not already have a home in 'NumHask.Free.Carriers' and
-- re-exports the off-the-shelf carriers we need.
module Circuit.Parser.Semiring
  ( -- * numhask semiring vocabulary (re-exported for convenience)
    NHR.StarSemiring,
    NHR.KleeneAlgebra,
    NHG.Idempotent,
    NHC.MinPlus (..),
    NHC.Warshall (..),

    -- * Local carriers
    Count (..),
    Dual2 (..),

    -- * Weighted source monad
    Weighted (..),
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Data.Kind (Type)
import NumHask.Algebra.Additive qualified as NHA
import NumHask.Algebra.Group qualified as NHG
import NumHask.Algebra.Multiplicative qualified as NHM
import NumHask.Algebra.Ring qualified as NHR
import NumHask.Free.Carriers qualified as NHC
import Prelude

-- | Counting semiring: number of derivations.  It is /not/ a Kleene algebra
-- because addition is not idempotent (@1 + 1 = 2@), so feedback cycles can
-- yield infinitely many derivations; the 'StarSemiring' instance handles only
-- the acyclic case.
newtype Count = Count Int
  deriving newtype (Eq, Ord, Num)
  deriving stock (Show)

instance NHA.Additive Count where
  zero = Count 0
  Count x + Count y = Count (x + y)

instance NHM.Multiplicative Count where
  one = Count 1
  Count x * Count y = Count (x * y)

instance NHR.StarSemiring Count where
  star (Count 0) = NHM.one
  star _ = error "Semiring.Count.star: infinite derivations"

-- | A dual number with two scalar parameters.  The first gradient component is
-- the partial derivative with respect to parameter A; the second with respect
-- to parameter B.  This is the semiring carrier that lets a weighted parser
-- return inside values and their parameter gradients simultaneously.
data Dual2 = Dual2
  { dualValue :: !Double,
    dualGradA :: !Double,
    dualGradB :: !Double
  }
  deriving (Eq, Show)

instance NHA.Additive Dual2 where
  zero = Dual2 0 0 0
  Dual2 v1 ga1 gb1 + Dual2 v2 ga2 gb2 =
    Dual2 (v1 + v2) (ga1 + ga2) (gb1 + gb2)

instance NHM.Multiplicative Dual2 where
  one = Dual2 1 0 0
  Dual2 v1 ga1 gb1 * Dual2 v2 ga2 gb2 =
    Dual2
      (v1 * v2)
      (v1 * ga2 + v2 * ga1)
      (v1 * gb2 + v2 * gb1)

instance NHR.StarSemiring Dual2 where
  star (Dual2 0 0 0) = NHM.one
  star _ = error "Semiring.Dual2.star: productive cycle"

-- | A list monad that carries a semiring weight with each result.  'return'
-- attaches 'one'; 'bind' multiplies weights along a derivation; 'mplus'
-- collects alternatives without merging them.  The target embedding sums
-- weights when the same output is reached by multiple derivations.
newtype Weighted (r :: Type) a = Weighted {runWeighted :: [(a, r)]}
  deriving stock (Eq, Show, Functor)

instance (NHM.Multiplicative r, NHA.Additive r) => Applicative (Weighted r) where
  pure a = Weighted [(a, NHM.one)]
  Weighted fs <*> Weighted xs =
    Weighted [(f x, wf NHM.* wx) | (f, wf) <- fs, (x, wx) <- xs]

instance (NHM.Multiplicative r, NHA.Additive r) => Monad (Weighted r) where
  Weighted xs >>= f =
    Weighted [(b, w1 NHM.* w2) | (a, w1) <- xs, (b, w2) <- runWeighted (f a)]

instance (NHM.Multiplicative r, NHA.Additive r) => Alternative (Weighted r) where
  empty = Weighted []
  Weighted xs <|> Weighted ys = Weighted (xs ++ ys)

instance (NHM.Multiplicative r, NHA.Additive r) => MonadPlus (Weighted r) where
  mzero = Weighted []
  mplus (Weighted xs) (Weighted ys) = Weighted (xs ++ ys)
