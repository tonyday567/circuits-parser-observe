{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Branching-trace probe for 'Loop Either'.
--
-- The standard 'Traced Either (Kleisli m)' instance iterates: it commits to
-- the first 'Right' result produced by the loop body and discards any
-- alternatives.  This is exactly why running a parser with 'LogicT' still
-- returns a singleton list when choices are expressed through the trace
-- (see "Circuit.Parser").
--
-- This module defines a small wrapper monad 'BranchingM' and a 'Traced'
-- instance that enumerates /all/ reachable 'Right' exits instead of stopping
-- at the first one.  It is a probe, not a polished semantics: lawfulness of
-- a branching trace is intentionally left open.
--
-- The probe also shows that making the trace branch is only half the story.
-- The parser's choice combinator must be willing to emit both a success /and/
-- a failure-feedback edge; the standard '('<|>')' commits on success and does
-- not.  'altB' below is a choice operator that runs the left parser to
-- completion before falling through to the right, giving LogicT-style
-- enumeration for finite left parses.
--
-- Repetition ('many' / 'some') is deliberately not provided here: the
-- obvious recursive definition leaks a failure branch because the existing
-- '<*>' reports the /original/ input on component failure.  Designing a
-- total branching repetition combinator is the next step after this probe.
module Circuit.Parser.BranchingTraceProbe
  ( BranchingM (..),
    runBranching,
    runLogic,
    altB,
    traceDemo,
    demo,
  )
where

import Circuit qualified as C
import Circuit.Channel (Traced (..))
import Circuit.Parser
  ( Parser (..),
    Uncons,
    runParser,
    string,
  )
import Control.Applicative (Alternative, empty)
import Control.Arrow (Kleisli (..))
import Control.Monad (MonadPlus)
import Control.Monad.Logic (LogicT, MonadLogic, interleave, msplit, observeAllT)
import Control.Monad.Trans (MonadTrans)
import Data.Functor.Identity (Identity, runIdentity)
import Data.These (These (..))

-- | A monad that exposes the branching structure of 'LogicT' so that the
-- 'Either' trace can resume after a successful exit.
newtype BranchingM m a = BranchingM {runBranchingM :: LogicT m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, Alternative, MonadPlus, MonadLogic)

-- | Enumerate every 'Right' result reachable by following 'Left' feedback
-- edges, rather than committing to the first one.
instance (Monad m) => Traced Either (Kleisli (BranchingM m)) where
  trace :: forall a b c. Kleisli (BranchingM m) (Either a b) (Either a c) -> Kleisli (BranchingM m) b c
  trace (Kleisli f) = Kleisli $ go . Right
    where
      go :: Either a b -> BranchingM m c
      go x = do
        mb <- msplit (f x)
        case mb of
          Nothing -> empty
          Just (r, rest) -> handle r `interleave` (rest >>= handle)

      handle :: Either a c -> BranchingM m c
      handle (Left a) = go (Left a)
      handle (Right c) = pure c

-- | Run a parser with the branching trace and collect every result.
runBranching ::
  (Monad m, Uncons f s) =>
  Parser (BranchingM m) f s a ->
  f ->
  m [These a f]
runBranching p f = observeAllT (runBranchingM (runParser p f))

-- | Run a parser in the naive @LogicT@ base monad and collect every result.
--
-- The standard 'Loop Either' control structure commits to the first
-- successful branch, so this usually returns a singleton list. It becomes a
-- list of alternatives only when the parser itself uses the base monad's
-- nondeterminism. 'runBranching' is the branching-trace fix that enumerates
-- all exits.
runLogic ::
  forall n f s a.
  (Monad n, Uncons f s) =>
  Parser (LogicT n) f s a ->
  f ->
  n [These a f]
runLogic p f = observeAllT (runParser p f)

-- | Branching choice: run the left parser to completion, emit each success,
-- then fall through to the right parser.  This is the LogicT-style
-- interpretation of '<|>', not the committing backtracking one.
altB ::
  forall m f s a.
  (Monad m, Uncons f s) =>
  Parser (BranchingM m) f s a ->
  Parser (BranchingM m) f s a ->
  Parser (BranchingM m) f s a
altB p1 p2 = Parser $ C.trace $ C.Lift $ Kleisli $ \case
  Right s -> leftSuccesses s `interleave` pure (Left s)
  Left s -> Right <$> runParser p2 s
  where
    leftSuccesses :: f -> BranchingM m (Either f (These a f))
    leftSuccesses s = do
      res <- runParser p1 s
      case res of
        That _ -> empty
        _ -> pure (Right res)

-- | Pure trace demonstration: a body that returns 'Right 1' and 'Left ()'
-- on the initial input, and 'Right 2' on the feedback channel.  The
-- iterating trace returns '[1]'; the branching trace returns '[1,2]'.
traceDemo :: [Int]
traceDemo = runIdentity $ observeAllT $ runBranchingM $ go ()
  where
    body :: Kleisli (BranchingM Identity) (Either () ()) (Either () Int)
    body = Kleisli $ \case
      Right () -> pure (Right 1) `interleave` pure (Left ())
      Left () -> pure (Right 2)

    go = runKleisli $ C.trace body

-- | Interactive smoke test for 'altB'.
--
-- >>> demo
-- [These "ab" "",These "a" "b"]
demo :: IO ()
demo = do
  let abOrA = string "ab" `altB` string "a" :: Parser (BranchingM IO) String Char String
  print =<< runBranching abOrA "ab"
