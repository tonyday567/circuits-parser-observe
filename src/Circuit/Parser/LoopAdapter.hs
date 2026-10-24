{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Adapter from a @Knot Either@-based choice back into the runtime 'Parser'.
--
-- The probes ('BranchingTraceProbe', 'LevelsT') express nondeterministic
-- choice through their own 'Trace' instances over @Knot Either@; this adapts
-- the resulting knot into the @Step (,) (,) f f (K m)@ 'Parser'.  It lives
-- here rather than in the core because the core parser never needs to
-- build a knot — this is probe plumbing.
module Circuit.Parser.LoopAdapter
  ( loopToParser,
  )
where

import Circuit qualified as C
import Circuit.Category (K (..), runK)
import Circuit.Parser (Parser (..), Uncons (..))
import Circuit.Step (Step (..))
import Circuit.Traced (Trace)
import Data.These (These (..))

-- | The stream leftover after a parse result: the new thread state.
leftoverOf :: forall f s a. (Uncons f s) => These a f -> f
leftoverOf (This _) = nil @f @s
leftoverOf (That f) = f
leftoverOf (These _ f) = f

-- | View a @Knot@-based parser as a @Step@-based 'Parser'.
loopToParser ::
  forall m f s a.
  (Monad m, Uncons f s, Trace Either (K m)) =>
  C.Knot Either (K m) f (These a f) ->
  Parser m f s a
loopToParser loop = Parser $ Step $ K $ \(f, ()) -> do
  r <- runK (C.run loop) f
  pure (leftoverOf @f @s r, r)
