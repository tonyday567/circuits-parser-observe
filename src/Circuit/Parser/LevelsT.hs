{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Fair breadth-first parser family via the @LevelsT@ transformer.
--
-- This is the Kidney & Wu "Algebras for Weighted Search" BFS layer applied to
-- the unified parser.  @LevelsT m a@ is a list of bags of @a@, ordered by the
-- number of steps needed to reach each outcome.  The 'Alternative' '<|>'
-- interleaves outcomes at the same level rather than left-biasing, and the
-- 'Traced' instance for @Kleisli (LevelsT m)@ enumerates /all/ reachable
-- 'Right' exits of an @Either@ loop instead of committing to the first one.
--
-- The implementation uses the Cayley/Church-encoded representation of
-- @LevelsT@ (Kidney & Wu, \u00a73.2).  This makes left-nested uses of '<|>'
-- cheap to construct, and the level-wise merge is the "zip" on the encoded
-- list implemented with the monadic hyperfunction @HypM@.
--
-- The standard 'Circuit.Parser.<|>' still commits on the first success,
-- because it does not emit a failure-feedback edge after a success.  Use
-- 'altLevels' for a choice operator that runs both branches and interleaves
-- their results fairly.
--
-- === doctests
--
-- >>> import Circuit.Parser
-- >>> import Circuit.Parser.LevelsT
-- >>> import Data.Functor.Identity (Identity, runIdentity)
-- >>> import Data.These (These (..))
--
-- >>> let p = string "ab" `altLevels` string "a" :: Parser (LevelsT Identity) String Char String
-- >>> runIdentity $ runParserLevelsT p "ab"
-- [These "ab" "",These "a" "b"]
module Circuit.Parser.LevelsT
  ( -- * Levels transformer
    LevelsT (..),
    Bag,
    observeLevelsT,

    -- * Fair parser choice and runner
    altLevels,
    runParserLevelsT,
  )
where

import Circuit qualified as C
import Circuit.Channel (Traced (..))
import Circuit.Parser
  ( Parser (..),
    Uncons,
    runParser,
  )
import Control.Applicative (Alternative (..))
import Control.Arrow (Kleisli (..))
import Control.Category ((.))
import Control.Monad (MonadPlus, ap, join)
import Control.Monad.Trans (MonadTrans (..))
import Data.These (These (..))
import Prelude hiding (id, (.))

-- | A bag is an unordered multiset.  We represent it as a list; ordering
-- within a bag is not semantically significant.
type Bag a = [a]

-- | The levels transformer: a breadth-first list of bags, each bag holding
-- the outcomes reachable in the same number of steps.
--
-- This is the Cayley/Church-encoded representation from Kidney & Wu: a value
-- of type @LevelsT m a@ is a fold over a list of bags of @a@, interspersed
-- with effects in @m@.  The explicit list-of-bags can be recovered via
-- 'observeLevelsT'.
newtype LevelsT m a = LevelsT
  { runLevelsT :: forall r. (Bag a -> m r -> m r) -> m r -> m r
  }

-- | Extract the outcomes level by level, flattening each bag in order.
observeLevelsT :: (Monad m) => LevelsT m a -> m [a]
observeLevelsT xs = runLevelsT xs go (pure [])
  where
    go bag rest = (bag ++) <$> rest

-- ---------------------------------------------------------------------------
-- LevelsT instances
-- ---------------------------------------------------------------------------

instance Functor (LevelsT m) where
  fmap f xs = LevelsT $ \c n ->
    runLevelsT xs (c . map f) n

instance (Monad m) => Applicative (LevelsT m) where
  pure a = LevelsT $ \c n -> c [a] n
  (<*>) = ap

instance (Monad m) => Monad (LevelsT m) where
  LevelsT xs >>= k = LevelsT $ \c n ->
    xs
      ( \bag rest ->
          runLevelsT (foldr (\a acc -> k a `altLevelsT` acc) emptyLevelsT bag) c rest
      )
      n

instance (Monad m) => Alternative (LevelsT m) where
  empty = emptyLevelsT
  (<|>) = altLevelsT

instance (Monad m) => MonadPlus (LevelsT m)

instance MonadTrans LevelsT where
  lift m = LevelsT $ \c n -> m >>= \a -> c [a] n

emptyLevelsT :: LevelsT m a
emptyLevelsT = LevelsT $ \_ n -> n

-- | Monadic hyperfunction, used for the asymptotically optimal level-wise
-- merge of two 'LevelsT' streams.
--
-- This is the @a \u21ac_m b@ type from Kidney & Wu (\u00a73.2): a hyperfunction
-- where the recursive step is wrapped in the base monad @m@ so that monadic
-- effects from the two merged streams can be interleaved.
newtype HypM m a b = HypM
  { invokeM :: m ((HypM m a b -> a) -> b)
  }

-- | Level-wise union ("zip with bag union") of two 'LevelsT' streams.
--
-- Implemented with the monadic hyperfunction zip from Kidney & Wu
-- (\u00a73.2).  The construction avoids the quadratic cost of a naive merge on
-- the Church encoding: '<|>' builds a composition of folds, and the actual
-- level-walking is pushed to observation time.
altLevelsT :: forall m a. (Monad m) => LevelsT m a -> LevelsT m a -> LevelsT m a
altLevelsT xs ys = LevelsT $ \(c :: Bag a -> m r -> m r) (n :: m r) ->
  let -- The empty left-stream hyperfunction.  When a folded right stream asks
      -- us to continue with an empty bag, we pass control back to that right
      -- consumer; when both sides are empty the right base ('emptyR') returns
      -- the nil @n@.
      nilL :: HypM m (Bag a -> m r) (m r)
      nilL = HypM $ pure $ \yk -> yk nilL []

      -- Base value for the right fold: the right stream is empty.  If the left
      -- stream is also empty at this level we return the nil; otherwise we emit
      -- the left bag and continue with the left tail.
      emptyR :: HypM m (Bag a -> m r) (m r) -> Bag a -> m r
      emptyR _ [] = n
      emptyR xk x = c x (invokeM xk >>= ($ emptyR))

      -- Fold the left stream into a producer hyperfunction.
      left :: m ((HypM m (Bag a -> m r) (m r) -> Bag a -> m r) -> m r)
      left =
        runLevelsT
          xs
          (\x xk -> pure $ \yk -> yk (HypM xk) x)
          (pure $ \yk -> yk nilL [])

      -- Fold the right stream into a consumer hyperfunction.
      right :: m (HypM m (Bag a -> m r) (m r) -> Bag a -> m r)
      right =
        runLevelsT
          ys
          (\y yk -> pure $ \xk x -> c (x ++ y) (join (invokeM xk <*> yk)))
          (pure emptyR)
   in join (left <*> right)

-- | Split a 'LevelsT' into its first outcome and the remainder.
msplitLevelsT :: (Monad m) => LevelsT m a -> m (Maybe (a, LevelsT m a))
msplitLevelsT xs = runLevelsT xs go (pure Nothing)
  where
    go [] rest = rest
    go (a : as) rest =
      pure
        ( Just
            ( a,
              LevelsT $ \c n -> c as (rest >>= maybe n (\(_, remainder) -> runLevelsT remainder c n))
            )
        )

-- ---------------------------------------------------------------------------
-- Branching Either trace for LevelsT
-- ---------------------------------------------------------------------------

-- | Enumerate every 'Right' result reachable by following 'Left' feedback
-- edges, interleaving the search breadth-first via 'LevelsT'.
instance (Monad m) => Traced Either (Kleisli (LevelsT m)) where
  trace (Kleisli f) = Kleisli (go . Right)
    where
      go x = do
        mb <- lift (msplitLevelsT (f x))
        case mb of
          Nothing -> empty
          Just (r, rest) -> handle r `altLevelsT` (rest >>= handle)

      handle (Left a) = go (Left a)
      handle (Right c) = pure c

-- ---------------------------------------------------------------------------
-- Fair parser choice and runner
-- ---------------------------------------------------------------------------

-- | Fair choice: emit every success from the left parser, and also fall
-- through to the right parser.  The two streams are interleaved level by
-- level.
altLevels ::
  (Monad m, Uncons f s) =>
  Parser (LevelsT m) f s a ->
  Parser (LevelsT m) f s a ->
  Parser (LevelsT m) f s a
altLevels p1 p2 = Parser $ C.trace $ C.Lift $ Kleisli $ \case
  Right s -> leftSuccesses s `altLevelsT` pure (Left s)
  Left s -> Right <$> runParser p2 s
  where
    leftSuccesses s = do
      res <- runParser p1 s
      case res of
        That _ -> empty
        _ -> pure (Right res)

-- | Run a parser with fair breadth-first enumeration and collect every
-- result.
runParserLevelsT ::
  (Monad m, Uncons f s) =>
  Parser (LevelsT m) f s a ->
  f ->
  m [These a f]
runParserLevelsT p f = observeLevelsT (runParser p f)
