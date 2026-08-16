{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Semiring-weighted interpretation of 'Loop Either' parsers.
--
-- This module integrates the cyclic semiring trace into the generic
-- 'Circuit.Layer.bind' fold.  The source parser is built over a constrained
-- Kleisli arrow 'CKleisli m Eq' so that feedback states can carry an 'Eq'
-- constraint; the target 'SemiringKleisli r' also has 'Ob = Eq'.  The
-- dictionary transformer between them is then the identity.
--
-- The payoff is that 'runSemiringParser' is literally 'C.bind phi emb': the
-- same parser syntax folds into a weighted relation whose 'Either' trace sums
-- over all paths through a cyclic choice loop.
--
-- The semiring vocabulary is 'numhask''s: 'Additive' / 'Multiplicative' /
-- 'StarSemiring'.  Carrier-dependent normalization mirrors
-- 'NumHask.Free.StarSemiring.kleeneSimplify': idempotent carriers (Kleene
-- algebras) merge duplicate outputs, counting carriers keep them.
module Circuit.Parser.SemiringProbe
  ( -- * Carriers and source monad
    Count (..),
    Dual2 (..),
    MinPlus (..),
    Weighted (..),

    -- * Constrained Kleisli source
    CKleisli (..),

    -- * Parser over constrained source
    EqParser (..),
    runEqParser,
    next,
    satisfy,
    satisfyW,
    char,
    charW,
    string,
    stringW,
    endOfInput,
    pureP,
    mapP,
    pairP,
    bindP,
    empty,
    (<|>),
    many,
    some,
    try,

    -- * Weighted relation target
    SemiringKleisli (..),
    altW,
    altWeighted,
    runSemiringParser,
    runWeightedParser,
    totalWeight,
    traceCyclic,

    -- * Normalization profile
    Normalizable (..),

    -- * Demos
    demo,
    cyclicDemo,
    manyCharA,
    manyCharAReach,
    outsideDemo,
    tropicalDemo,
  )
where

import Circuit qualified as C
import Circuit.Category (Category (..), ObDict (..), (.>))
import Circuit.Channel (Channel (..), Strength (..), Traced (..))
import Circuit.Parser (Uncons (..))
import Circuit.Parser.Semiring
  ( Count (..),
    Dual2 (..),
    MinPlus (..),
    Weighted (..),
  )
import Circuit.Tensor (Action (..), Tensor (..))
import Control.Monad (MonadPlus (..), (<=<))
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Kind (Constraint, Type)
import Data.List (elemIndex, sortOn)
import Data.Maybe (fromMaybe)
import Data.These (These (..), these)
import Circuit.Parser.Matrix (fromLists, starMatrix, toLists)
import NumHask.Algebra.Additive qualified as Add
import NumHask.Algebra.Multiplicative qualified as Mul
import NumHask.Algebra.Ring qualified as Ring
import Prelude hiding (id, (.))

-- ---------------------------------------------------------------------------
-- Carrier-dependent normalization (K&W quotient discipline)
-- ---------------------------------------------------------------------------

-- | Carriers choose how their weighted outputs are normalized.
--
-- 'kleeneSimplify' is sound only for idempotent carriers: merging duplicates
-- with '(+)' does not change the value.  For counting carriers like 'Count'
-- or 'Dual2', duplicates must be kept so that each derivation contributes
-- its weight.
class (Add.Additive r, Eq r) => Normalizable r where
  normalize :: (Eq b) => [(b, r)] -> [(b, r)]

-- | Drop zero-weight outputs.
delOnly :: (Add.Additive r, Eq r) => [(b, r)] -> [(b, r)]
delOnly = filter ((/= Add.zero) . snd)

-- | Merge duplicate outputs with '(+)' and drop zeros.
dupDel :: (Eq b, Add.Additive r, Eq r) => [(b, r)] -> [(b, r)]
dupDel = delOnly . foldl' insert []
  where
    insert [] y = [y]
    insert ((x, w) : xs) (y, wy)
      | x == y = (x, w Add.+ wy) : xs
      | otherwise = (x, w) : insert xs (y, wy)

instance Normalizable Bool where normalize = dupDel

instance Normalizable Count where normalize = delOnly

instance Normalizable Dual2 where normalize = delOnly

instance Normalizable (MinPlus Double) where normalize = dupDel

-- ---------------------------------------------------------------------------
-- Constrained Kleisli arrow
-- ---------------------------------------------------------------------------

-- | 'Kleisli' arrows whose objects must satisfy a constraint @c@.
--
-- This is the least-invasive way to make the parser source category carry a
-- non-trivial object constraint: the feedback state @s@ in a 'Loop.Knot' now
-- comes with 'Eq s' evidence, which the target semiring category needs.
newtype CKleisli m (c :: Type -> Constraint) a b = CKleisli
  { runCKleisli :: a -> m b
  }

instance (Monad m) => Category (CKleisli m c) where
  type Ob (CKleisli m c) a = c a
  id = CKleisli pure
  CKleisli f . CKleisli g = CKleisli (f <=< g)

instance (Monad m) => Channel (,) (CKleisli m Eq) where
  assoc = CKleisli $ pure . \((a, b), c) -> (a, (b, c))
  assoc' = CKleisli $ pure . \(a, (b, c)) -> ((a, b), c)
  slide = CKleisli $ pure . \(a, (b, c)) -> (b, (a, c))
  withTensorOb ObDict ObDict k = k

instance (Monad m) => Channel Either (CKleisli m Eq) where
  assoc =
    CKleisli $
      pure . \case
        Left (Left a) -> Left a
        Left (Right b) -> Right (Left b)
        Right c -> Right (Right c)
  assoc' =
    CKleisli $
      pure . \case
        Left a -> Left (Left a)
        Right (Left b) -> Left (Right b)
        Right (Right c) -> Right c
  slide =
    CKleisli $
      pure . \case
        Left a -> Right (Left a)
        Right (Left b) -> Left b
        Right (Right c) -> Right (Right c)
  withTensorOb ObDict ObDict k = k

instance (Monad m) => Strength (,) (CKleisli m Eq) where
  strength (CKleisli f) = CKleisli $ \(a, b) -> f b >>= \c -> pure (a, c)
  withStrengthOb ObDict ObDict ObDict k = k

instance (Monad m) => Strength Either (CKleisli m Eq) where
  strength (CKleisli f) = CKleisli $ \case
    Left a -> pure (Left a)
    Right b -> f b >>= \c -> pure (Right c)
  withStrengthOb ObDict ObDict ObDict k = k

instance (Monad m) => Traced Either (CKleisli m Eq) where
  trace (CKleisli f) = CKleisli $ \b -> go (Right b)
    where
      go x =
        f x >>= \case
          Right c -> pure c
          Left a -> go (Left a)

-- ---------------------------------------------------------------------------
-- Parser over the constrained source
-- ---------------------------------------------------------------------------

-- | Parser syntax over @Loop Either (CKleisli m Eq)@.
--
-- The 'Eq' constraint is paid only when the parser will be interpreted through
-- a target that needs equality on feedback states (e.g. cyclic semiring
-- parsing).  The standard 'Parser' over 'Kleisli m' remains unconstrained.
newtype EqParser m f s a = EqParser
  { unEqParser :: C.Loop Either (CKleisli m Eq) f (These a f)
  }

-- | Run a parser in the base monad.
runEqParser :: forall m f s a. (Monad m, Eq f, Eq a) => EqParser m f s a -> f -> m (These a f)
runEqParser p = runCKleisli (C.run (unEqParser p))

-- | Consume and return the next element, or 'That' if the stream is empty.
next :: forall m f s. (Monad m, Uncons f s) => EqParser m f s s
next = EqParser $ C.Lift $ CKleisli $ \f -> pure (uncons f)

-- | Apply a predicate to the result of 'uncons'.
guardThese :: (Uncons f s) => (s -> Bool) -> f -> These s f -> These s f
guardThese p def =
  these
    (\a -> bool (That def) (This a) (p a))
    That
    (\a b -> bool (That def) (These a b) (p a))

-- | True for failure results.
isThat :: These a f -> Bool
isThat = these (const False) (const True) (\_ _ -> False)

-- | Consume one element if it satisfies the predicate.
satisfy :: forall m f s. (Monad m, Uncons f s) => (s -> Bool) -> EqParser m f s s
satisfy p = EqParser $ C.Lift $ CKleisli $ \f -> pure (guardThese p f (uncons f))

-- | Match a specific element.
char :: forall m f s. (Monad m, Uncons f s, Eq s) => s -> EqParser m f s s
char c = satisfy (== c)

-- | Match a sequence of elements.
string :: forall m f s. (Monad m, Uncons f s, Eq s, Eq f) => [s] -> EqParser m f s [s]
string = go []
  where
    go :: [s] -> [s] -> EqParser m f s [s]
    go acc [] = pureP (reverse acc)
    go acc (c : cs) = bindP (char c) (\x -> go (x : acc) cs)

-- | Consume one element and attach a semiring weight.
--
-- A weight of 'zero' is treated as failure: no result is emitted.
satisfyW ::
  forall f s r.
  (Uncons f s, Add.Additive r, Eq r) =>
  (s -> r) ->
  EqParser (Weighted r) f s s
satisfyW w = EqParser $ C.Lift $ CKleisli $ \f ->
  Weighted $ case uncons f of
    That _ -> []
    This s -> [(This s, w s) | w s /= Add.zero]
    These s f' -> [(These s f', w s) | w s /= Add.zero]

-- | Match a specific element, weighted.
charW ::
  forall f s r.
  (Uncons f s, Eq s, Add.Additive r, Eq r) =>
  (s -> r) ->
  s ->
  EqParser (Weighted r) f s s
charW w c = satisfyW (\s -> if s == c then w s else Add.zero)

-- | Match a sequence of elements, weighted.
stringW ::
  forall f s r.
  (Uncons f s, Eq s, Eq f, Add.Additive r, Eq r, Mul.Multiplicative r) =>
  (s -> r) ->
  [s] ->
  EqParser (Weighted r) f s [s]
stringW w = go []
  where
    go :: [s] -> [s] -> EqParser (Weighted r) f s [s]
    go acc [] = pureP (reverse acc)
    go acc (c : cs) = bindP (charW w c) (\x -> go (x : acc) cs)

-- | Succeed only at the end of input.
endOfInput :: forall m f s. (Monad m, Uncons f s) => EqParser m f s ()
endOfInput = EqParser $ C.Lift $ CKleisli $ \f ->
  pure $ case uncons @f @s f of
    That _ -> These () f
    _ -> That f

-- | Functorial map.  The result type must be 'Eq' because the underlying
-- traced category carries an object-level equality constraint.
mapP ::
  (Monad m, Eq f, Eq a, Eq b) =>
  (a -> b) ->
  EqParser m f s a ->
  EqParser m f s b
mapP g (EqParser p) = EqParser (p .> C.Lift (CKleisli (pure . first g)))

-- | Lift a pure value into a parser that consumes nothing.
pureP :: (Monad m, Eq f, Eq a) => a -> EqParser m f s a
pureP a = EqParser $ C.Lift $ CKleisli $ \f -> pure (These a f)

-- | Sequential composition that threads the stream and pairs the results.
--
-- This is the applicative spine without the function-valued intermediate
-- object that makes a generic '<*>' impossible under an 'Eq' object
-- constraint.
pairP ::
  forall m f s a b.
  (Monad m, Uncons f s, Eq f, Eq a, Eq b) =>
  EqParser m f s a ->
  EqParser m f s b ->
  EqParser m f s (a, b)
pairP (EqParser p) (EqParser q) = EqParser $ C.Lift $ CKleisli $ \s -> do
  runCKleisli (C.run p) s >>= \case
    That s' -> pure (That s')
    This a ->
      runCKleisli (C.run q) (nil @f @s) >>= \case
        That _ -> pure (That s)
        This b -> pure (These (a, b) (nil @f @s))
        These b s'' -> pure (These (a, b) s'')
    These a s' ->
      runCKleisli (C.run q) s' >>= \case
        That _ -> pure (That s)
        This b -> pure (These (a, b) (nil @f @s))
        These b s'' -> pure (These (a, b) s'')

-- | Monadic bind.  The continuation is applied after the first parser
-- succeeds, so the intermediate value never appears as a feedback object.
bindP ::
  forall m f s a b.
  (Monad m, Uncons f s, Eq f, Eq a, Eq b) =>
  EqParser m f s a ->
  (a -> EqParser m f s b) ->
  EqParser m f s b
bindP (EqParser m) k = EqParser $ C.Lift $ CKleisli $ \s -> do
  runCKleisli (C.run m) s >>= \case
    That s' -> pure (That s')
    This a -> runCKleisli (C.run (unEqParser (k a))) (nil @f @s)
    These a s' -> runCKleisli (C.run (unEqParser (k a))) s'

-- | A parser that always fails without consuming input.
empty :: (MonadPlus m, Eq f, Eq a) => EqParser m f s a
empty = EqParser $ C.Lift $ CKleisli $ const mzero

-- | Traced choice: collect successes from both branches.
--
-- Because the source category is constrained by 'Eq', the result type must
-- also be 'Eq' here.  The feedback state is just the stream, so the cyclic
-- trace can compare it.
--
-- The branching is expressed with 'mplus', so this works for any
-- 'MonadPlus' source (in particular the list monad used by
-- 'runSemiringParser').
(<|>) ::
  forall m f s a.
  (MonadPlus m, Uncons f s, Eq f, Eq a) =>
  EqParser m f s a ->
  EqParser m f s a ->
  EqParser m f s a
EqParser p1 <|> EqParser p2 = EqParser $ C.trace $ C.Lift $ CKleisli $ \case
  Right s -> do
    res <- runCKleisli (C.run p1) s
    case res of
      That s' -> pure (Left s')
      _ -> pure (Right res) `mplus` pure (Left s)
  Left s -> Right <$> runCKleisli (C.run p2) s

-- | Zero or more repetitions.
--
-- Implemented as a single feedback loop over @(f, [a])@: each successful
-- step of @p@ both emits an exit (the accumulated list) and continues
-- with one more element.  This keeps every derivation counted exactly once.
many ::
  forall m f s a.
  (MonadPlus m, Uncons f s, Eq f, Eq a) =>
  EqParser m f s a ->
  EqParser m f s [a]
many p = EqParser $ C.trace $ C.Lift $ CKleisli $ \case
  Right f -> pure (Left (f, []))
  Left (f, acc) ->
    let exit = pure (Right (These (reverse acc) f))
     in runCKleisli (C.run (unEqParser p)) f >>= \case
          That _ -> exit
          This a ->
            let acc' = a : acc
             in exit `mplus` pure (Left (nil @f @s, acc'))
          These a f' ->
            let acc' = a : acc
             in exit `mplus` pure (Left (f', acc'))

-- | One or more repetitions.
some ::
  forall m f s a.
  (MonadPlus m, Uncons f s, Eq f, Eq a) =>
  EqParser m f s a ->
  EqParser m f s [a]
some p = mapP (uncurry (:)) (pairP p (many p))

-- | Attempt a parser. If it fails with 'That', restore the original stream.
try :: forall m f s a. (Monad m, Uncons f s, Eq f, Eq a) => EqParser m f s a -> EqParser m f s a
try (EqParser p) = EqParser $ C.Lift $ CKleisli $ \s -> do
  res <- runCKleisli (C.run p) s
  pure $ case res of
    That _ -> That s
    result -> result

-- ---------------------------------------------------------------------------
-- Weighted relation arrow
-- ---------------------------------------------------------------------------

-- | A morphism @a -> b@ is a weighted relation: for each input it produces a
-- finite bag of outputs together with their weights.
newtype SemiringKleisli r a b = SemiringKleisli
  { runSemiringKleisli :: a -> [(b, r)]
  }

instance (Mul.Multiplicative r, Normalizable r) => Category (SemiringKleisli r) where
  type Ob (SemiringKleisli r) a = Eq a
  id = SemiringKleisli $ \a -> [(a, Mul.one)]
  SemiringKleisli g . SemiringKleisli f =
    SemiringKleisli $ \a ->
      normalize
        [ (c, w1 Mul.* w2)
        | (b, w1) <- f a,
          (c, w2) <- g b
        ]

instance (Mul.Multiplicative r, Normalizable r) => Channel (,) (SemiringKleisli r) where
  assoc = SemiringKleisli $ \((a, b), c) -> [((a, (b, c)), Mul.one)]
  assoc' = SemiringKleisli $ \(a, (b, c)) -> [(((a, b), c), Mul.one)]
  slide = SemiringKleisli $ \(a, (b, c)) -> [((b, (a, c)), Mul.one)]
  withTensorOb ObDict ObDict k = k

instance (Mul.Multiplicative r, Normalizable r) => Channel Either (SemiringKleisli r) where
  assoc = SemiringKleisli $ \case
    Left (Left a) -> [(Left a, Mul.one)]
    Left (Right b) -> [(Right (Left b), Mul.one)]
    Right c -> [(Right (Right c), Mul.one)]
  assoc' = SemiringKleisli $ \case
    Left a -> [(Left (Left a), Mul.one)]
    Right (Left b) -> [(Left (Right b), Mul.one)]
    Right (Right c) -> [(Right c, Mul.one)]
  slide = SemiringKleisli $ \case
    Left a -> [(Right (Left a), Mul.one)]
    Right (Left b) -> [(Left b, Mul.one)]
    Right (Right c) -> [(Right (Right c), Mul.one)]
  withTensorOb ObDict ObDict k = k

instance (Mul.Multiplicative r, Normalizable r) => Strength (,) (SemiringKleisli r) where
  strength f = SemiringKleisli $ \(a, b) ->
    [ ((a, c), w) | (c, w) <- runSemiringKleisli f b
    ]
  withStrengthOb ObDict ObDict ObDict k = k

instance (Mul.Multiplicative r, Normalizable r) => Strength Either (SemiringKleisli r) where
  strength f = SemiringKleisli $ \case
    Left a -> [(Left a, Mul.one)]
    Right b ->
      [ (Right c, w) | (c, w) <- runSemiringKleisli f b
      ]
  withStrengthOb ObDict ObDict ObDict k = k

instance (Mul.Multiplicative r, Normalizable r) => Tensor (,) (SemiringKleisli r) where
  par f g = SemiringKleisli $ \(a, c) ->
    [ ((b, d), w1 Mul.* w2)
    | (b, w1) <- runSemiringKleisli f a,
      (d, w2) <- runSemiringKleisli g c
    ]
  unitl = SemiringKleisli $ \((), a) -> [(a, Mul.one)]
  unitl' = SemiringKleisli $ \a -> [(((), a), Mul.one)]
  unitr = SemiringKleisli $ \(a, ()) -> [(a, Mul.one)]
  unitr' = SemiringKleisli $ \a -> [((a, ()), Mul.one)]

instance (Mul.Multiplicative r, Normalizable r) => Action (,) (SemiringKleisli r) where
  swap = SemiringKleisli $ \(a, b) -> [((b, a), Mul.one)]

sumR :: (Add.Additive r) => [r] -> r
sumR = foldr (Add.+) Add.zero

-- ---------------------------------------------------------------------------
-- Square matrix toolbox for Kleene star
-- ---------------------------------------------------------------------------
--
-- The canonical matrix carrier lives in "Circuit.Mat.Dense".
feedbackClosure ::
  (Eq a) =>
  (a -> [(a, r)]) ->
  [a] ->
  [a]
feedbackClosure edges = go []
  where
    go seen [] = seen
    go seen (x : xs)
      | x `elem` seen = go seen xs
      | otherwise =
          let frontier = map fst (edges x)
           in go (x : seen) (xs ++ frontier)

-- | Cyclic trace for 'SemiringKleisli'.
--
-- Computes the reflexive-transitive closure of the feedback-to-feedback graph
-- over a 'StarSemiring'.  This is now the lawful 'Traced Either' instance for
-- the category, enabled by the constrained-bind refactor.
traceCyclic ::
  forall r a b c.
  (Ring.StarSemiring r, Normalizable r, Eq a, Eq b, Eq c) =>
  SemiringKleisli r (Either a b) (Either a c) ->
  SemiringKleisli r b c
traceCyclic (SemiringKleisli f) = SemiringKleisli $ \b0 -> solve (Right b0)
  where
    solve :: Either a b -> [(c, r)]
    solve start =
      let startEdges = f start
          startLefts = [(a, w) | (Left a, w) <- startEdges]
          startOuts = [(c, w) | (Right c, w) <- startEdges]
          feedbackEdges a' = [(a'', w) | (Left a'', w) <- f (Left a')]
          leftStates = feedbackClosure feedbackEdges (map fst startLefts)
          idx a = fromMaybe (error "traceCyclic: feedback state escaped closure") (elemIndex a leftStates)
          n = length leftStates
          adj =
            fromLists
              [ [ sumR [w | (Left a', w) <- f (Left ai), a' == aj]
                | aj <- leftStates
                ]
              | ai <- leftStates
              ]
          closure = toLists (starMatrix adj)
          leftOuts j =
            [ (outC, w)
            | (Right outC, w) <- f (Left (leftStates !! j))
            ]
          rawOuts =
            startOuts
              ++ [ (c, w1 Mul.* (closure !! i !! j) Mul.* w2)
                 | (a, w1) <- startLefts,
                   let i = idx a,
                   j <- [0 .. n - 1],
                   (c, w2) <- leftOuts j
                 ]
       in normalize rawOuts

instance (Ring.StarSemiring r, Normalizable r) => Traced Either (SemiringKleisli r) where
  trace = traceCyclic

-- ---------------------------------------------------------------------------
-- Source parser choice and runner
-- ---------------------------------------------------------------------------

-- | Weighted choice for a list-monad 'EqParser'.
--
-- The body collects all successes of the left branch and emits a single
-- feedback edge to the right branch, so alternatives are counted exactly
-- once rather than once per left success.
altW ::
  forall f s a.
  (Eq f, Eq a) =>
  EqParser [] f s a ->
  EqParser [] f s a ->
  EqParser [] f s a
altW (EqParser p1) (EqParser p2) = EqParser $ C.trace $ C.Lift $ CKleisli $ \case
  Right s ->
    let ress = runCKleisli (C.run p1) s
        successes = [res | res <- ress, not (isThat res)]
     in Left s : map Right successes
  Left s -> map Right (runCKleisli (C.run p2) s)

-- | Weighted choice for a 'Weighted' source monad.
--
-- Like 'altW', but preserves primitive weights: each left success exits with
-- its own weight, and the fallback to the right branch is emitted with weight
-- 'one'.  This is the correct semiring alternative for weighted parsers; the
-- generic '(<|>)' inherits the success weight on its fallback edge and
-- overcounts.
altWeighted ::
  forall f s a r.
  (Eq f, Eq a, Add.Additive r, Mul.Multiplicative r) =>
  EqParser (Weighted r) f s a ->
  EqParser (Weighted r) f s a ->
  EqParser (Weighted r) f s a
altWeighted (EqParser p1) (EqParser p2) = EqParser $ C.trace $ C.Lift $ CKleisli $ \case
  Right s ->
    let Weighted ress = runCKleisli (C.run p1) s
        successes = [(res, w) | (res, w) <- ress, not (isThat res)]
     in Weighted $ (Left s, Mul.one) : map (first Right) successes
  Left s ->
    Weighted $ map (first Right) (runWeighted (runCKleisli (C.run p2) s))

-- | Natural transformation from source to target dictionaries.
--
-- Both categories have 'Ob = Eq', so the transformer is the identity.
eqDict :: ObDict (CKleisli [] Eq) a -> ObDict (SemiringKleisli r) a
eqDict ObDict = ObDict

-- | Interpret an 'EqParser' into the weighted target and collect the weighted
-- results for a given input.  This now uses the generic 'C.bind' fold with the
-- cyclic 'Traced Either' instance, so unbounded repetition is summed rather
-- than divergent.
runSemiringParser ::
  forall r f s a.
  (Ring.StarSemiring r, Normalizable r, Uncons f s, Eq f, Eq s, Eq a) =>
  EqParser [] f s a ->
  f ->
  [(These a f, r)]
runSemiringParser p =
  runSemiringKleisli (C.bind eqDict emb (unEqParser p))
  where
    emb :: CKleisli [] Eq C.:~> SemiringKleisli r
    emb (CKleisli g) = SemiringKleisli $ \x -> [(y, Mul.one) | y <- g x]

-- | Natural transformation from the weighted source to the weighted target.
eqDictWeighted :: ObDict (CKleisli (Weighted r) Eq) a -> ObDict (SemiringKleisli r) a
eqDictWeighted ObDict = ObDict

-- | Interpret a weighted 'EqParser' into the weighted relation target.
-- Each primitive contributes its own weight; alternatives sum over paths.
runWeightedParser ::
  forall r f s a.
  (Ring.StarSemiring r, Normalizable r, Uncons f s, Eq f, Eq s, Eq a) =>
  EqParser (Weighted r) f s a ->
  f ->
  [(These a f, r)]
runWeightedParser p =
  runSemiringKleisli (C.bind eqDictWeighted emb (unEqParser p))
  where
    emb :: CKleisli (Weighted r) Eq C.:~> SemiringKleisli r
    emb (CKleisli g) = SemiringKleisli (runWeighted . g)

-- | Sum the weights for all outputs, ignoring the outputs themselves.
totalWeight :: (Add.Additive r) => [(x, r)] -> r
totalWeight = foldr ((Add.+) . snd) Add.zero

-- | Smoke test: count the parses of @"ab" | "a"@ on input @"ab"@.
--
-- >>> demo
-- [(These "ab" "",Count 1),(These "a" "b",Count 1)]
-- Count 2
demo :: IO ()
demo = do
  let p = string "ab" `altW` string "a" :: EqParser [] String Char String
  let results = runSemiringParser @Count p "ab"
  print results
  print $ totalWeight results

-- | Cyclic trace smoke test: a loop that can either exit immediately or
-- spin arbitrarily many times before exiting.  With the 'Bool' semiring the
-- answer is simply "an exit is reachable".
--
-- >>> cyclicDemo
-- True
cyclicDemo :: Bool
cyclicDemo = any snd (runSemiringKleisli (traceCyclic body) ())
  where
    body :: SemiringKleisli Bool (Either () ()) (Either () ())
    body = SemiringKleisli $ \case
      Right () -> [(Left (), True), (Right (), True)]
      Left () -> [(Left (), True), (Right (), True)]

-- | A more parser-like cyclic relation: @many (char 'a')@ over a 'String'
-- stream.  The feedback channel carries both the remaining stream and the
-- accumulated prefix, so each exit records exactly how many @a@s were
-- consumed.  This exercises 'traceCyclic' with a non-trivial feedback state
-- and the 'Count' semiring.
--
-- >>> manyCharA "aab"
-- [(These "" "aab",Count 1),(These "a" "ab",Count 1),(These "aa" "b",Count 1)]
manyCharA :: String -> [(These [Char] String, Count)]
manyCharA s = sortOn fst (runSemiringParser p s)
  where
    p :: EqParser [] String Char [Char]
    p = many (char 'a')

-- | Reachability version of 'manyCharA': is there /any/ parse of the loop?
--
-- >>> manyCharAReach "aab"
-- True
manyCharAReach :: String -> Bool
manyCharAReach s = any snd (runSemiringParser @Bool p s)
  where
    p :: EqParser [] String Char [Char]
    p = many (char 'a')

-- ---------------------------------------------------------------------------
-- Differentiable semiring carrier: outside = backprop of inside
-- ---------------------------------------------------------------------------

-- | Weight function for a two-token alphabet.  Parameter A controls the weight
-- of 'a'; parameter B controls the weight of 'b'.
abWeights :: Char -> Dual2
abWeights 'a' = Dual2 2 1 0
abWeights 'b' = Dual2 3 0 1
abWeights _ = Add.zero

-- | Outside = backprop-of-inside smoke test.
--
-- Parser: "ab" | "a" on input "ab".  The inside value is @wa * wb@; the
-- gradients are @wb@ w.r.t. @wa@ and @wa@ w.r.t. @wb@.  Those gradients are
-- exactly the outside values: the total weight of all derivations that use
-- the respective primitive.
--
-- With the test weights @wa = 2@, @wb = 3@ the inside value is 6 and the
-- outside values are 3 and 2.
--
-- >>> outsideDemo
-- Dual2 {dualValue = 6.0, dualGradA = 3.0, dualGradB = 2.0}
-- Dual2 {dualValue = 2.0, dualGradA = 1.0, dualGradB = 0.0}
outsideDemo :: IO ()
outsideDemo = do
  let p = stringW abWeights "ab" `altWeighted` stringW abWeights "a" :: EqParser (Weighted Dual2) String Char String
  case runWeightedParser p "ab" of
    [(These "ab" "", d), (These "a" "b", d')] -> do
      print d
      print d'
    other -> print other

-- ---------------------------------------------------------------------------
-- Tropical semiring carrier: Viterbi / best-first parsing
-- ---------------------------------------------------------------------------

-- | Cost function for a two-token alphabet.
abCosts :: Char -> MinPlus Double
abCosts 'a' = MinPlus 1.5
abCosts 'b' = MinPlus 2.0
abCosts _ = Add.zero

-- | Tropical / Viterbi smoke test.
--
-- Parser: "ab" | "a" on input "ab".  The derivation costs are the sums of
-- primitive costs; the tropical inside value is the minimum.  Sorting the
-- results by cost gives a min-cost-first view of the parse forest.
--
-- >>> tropicalDemo
-- [(These "a" "b",MinPlus {getMinPlus = 1.5}),(These "ab" "",MinPlus {getMinPlus = 3.5})]
tropicalDemo :: IO ()
tropicalDemo = do
  let p = stringW abCosts "ab" `altWeighted` stringW abCosts "a" :: EqParser (Weighted (MinPlus Double)) String Char String
  print $ sortOn snd (runWeightedParser p "ab")
