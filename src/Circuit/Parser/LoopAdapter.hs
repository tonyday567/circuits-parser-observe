{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Adapter from a @Loop Either@-based choice back into the runtime 'Parser'.
--
-- The probes ('BranchingTraceProbe', 'LevelsT') express nondeterministic
-- choice through their own 'Traced' instances over @Loop Either@; this adapts
-- the resulting loop morphism into the @Thread (,) (Kleisli m) f@ 'Parser'.
-- It lives here rather than in the core because the core parser never needs
-- to build a 'Loop' — this is probe plumbing.
module Circuit.Parser.LoopAdapter
  ( loopToParser,
  )
where

import Circuit qualified as C
import Circuit.Channel (Traced)
import Circuit.Parser (Parser (..), Uncons (..))
import Circuit.Thread (Thread (..))
import Control.Arrow (Kleisli (..))
import Data.These (These (..))

-- | The stream leftover after a parse result: the new thread state.
leftoverOf :: forall f s a. (Uncons f s) => These a f -> f
leftoverOf (This _) = nil @f @s
leftoverOf (That f) = f
leftoverOf (These _ f) = f

-- | View a @Loop@-based parser as a @Thread@-based 'Parser'.
loopToParser ::
  forall m f s a.
  (Monad m, Uncons f s, Traced Either (Kleisli m)) =>
  C.Loop Either (Kleisli m) f (These a f) ->
  Parser m f s a
loopToParser loop = Parser $ Thread $ Kleisli $ \(f, ()) -> do
  r <- runKleisli (C.run loop) f
  pure (leftoverOf @f @s r, r)
