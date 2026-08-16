{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Process-machine bridge: parsing as a coalgebraic state machine.
--
-- The semiring spike resolved the branching-trace gate algebraically, by
-- summing over all paths through a cyclic choice graph ('SemiringProbe').  The
-- @circuits-stats@ package contains the same phenomenon from the coalgebra side:
--
-- * 'Circuit.Stats.Trace' gives a lazy @Traced (,) Process@ instance.
-- * 'Circuit.Stats.Diff' gives reverse-mode AD through a Process scan via
--   'DiffProcess'.
--
-- This probe shows that a tiny parser can be written as a state machine, and
-- that the machine's parameter gradients coincide with the outside values
-- computed by the semiring parser.  It also adds a soft-stack pushdown parser
-- — the coalgebraic analogue of DuSell & Chiang's differentiable stack
-- machine.
--
-- === Trace-divergence correspondence
--
-- 'Circuit.Stats.Trace' diverges for strict numeric accumulators (moving
-- average) because the per-step fixed point @a_t = r * a_t + x_t@ has no lazy
-- solution.  That is the same obstruction as the naive @Traced Either@ parser
-- committing to the first successful branch: some feedbacks do not admit the
-- obvious trace.  The replacements are 'DiffProcess''s reverse step and
-- 'SemiringProbe.traceCyclic''s Kleene closure respectively.
module Circuit.Parser.ProcessProbe
  ( -- * Token wrapper for non-differentiable inputs
    Token (..),

    -- * Plain Process recognizer
    abProcess,

    -- * Weighted DiffProcess recognizer and gradient bridge
    ABParam (..),
    ABState (..),
    ABMode (..),
    abDiffProcess,
    paramFold,
    paramScan,
    processOutsideDemo,

    -- * Soft-stack pushdown parser
    StackParam (..),
    StackState (..),
    softStackProcess,
    softStackDemo,
  )
where

import NumHask.Algebra.Additive qualified as Add
import NumHask.Algebra.Multiplicative qualified as Mul
import Circuit.Diff (Diff' (..))
import Circuit.Stats (Process (..))
import Circuit.Stats.Diff (DiffProcess (..), diffFold, diffScan)
import Prelude

-- $setup
-- >>> import Circuit.Stats (fold, scan)

-- ---------------------------------------------------------------------------
-- Non-differentiable input tokens
-- ---------------------------------------------------------------------------

-- | A character wrapped so that it can travel through 'DiffProcess' inputs.
--
-- The character is not a parameter we want gradients for, so addition is
-- defined to ignore gradients and keep the original token.
newtype Token = Token {unToken :: Char}
  deriving newtype (Eq, Show)

instance Add.Additive Token where
  zero = Token '\0'
  Token _ + Token c = Token c

-- ---------------------------------------------------------------------------
-- Plain Process recognizer for "ab" | "a"
-- ---------------------------------------------------------------------------

-- | Finite-state recognizer for the language @"ab" | "a"@.
--
-- >>> scan abProcess "ab"
-- [Just "a",Just "ab"]
--
-- >>> scan abProcess "b"
-- [Nothing]
--
-- >>> fold abProcess "aab"
-- Nothing
abProcess :: Process Char (Maybe String)
abProcess = Process inject step id
  where
    inject 'a' = Just "a"
    inject _ = Nothing
    step (Just "a") 'b' = Just "ab"
    step _ _ = Nothing

-- ---------------------------------------------------------------------------
-- Weighted DiffProcess recognizer: outside = backprop
-- ---------------------------------------------------------------------------

-- | Two scalar weights: @wa@ for matching @'a'@, @wb@ for matching @'b'@.
data ABParam = ABParam
  { abWa :: !Double,
    abWb :: !Double
  }
  deriving (Eq, Show)

instance Add.Additive ABParam where
  zero = ABParam 0 0
  ABParam a b + ABParam a' b' = ABParam (a + a') (b + b')

-- | Discrete control state of the recognizer.
data ABMode = Start | SawA | SawAB | Rejected
  deriving (Eq, Show)

-- | State of the weighted recognizer: accumulated weight plus control mode.
data ABState = ABState
  { abWeight :: !Double,
    abMode :: !ABMode
  }
  deriving (Eq, Show)

instance Add.Additive ABState where
  zero = ABState 0 Start
  ABState w m + ABState w' _ = ABState (w + w') m

-- | Weighted recognizer for @"ab" | "a"@ as a 'DiffProcess'.
--
-- The parameters are supplied at every input step; the same parameter value is
-- reused and the per-step gradients are summed by 'paramFold' to obtain the
-- total gradient.  This is exactly the outside = backprop-of-inside check run
-- through a coalgebraic target.
abDiffProcess :: DiffProcess ABState (ABParam, Token) Double
abDiffProcess =
  DiffProcess
    { dInject = Diff' $ \(p, Token c) -> case c of
        'a' ->
          ( ABState (abWa p) SawA,
            \(ABState dw' _) -> (ABParam dw' 0, Token '\0')
          )
        _ ->
          ( ABState 0 Rejected,
            const (ABParam 0 0, Token '\0')
          ),
      dStep = Diff' $ \(ABState w m, (p, Token c)) -> case (m, c) of
        (SawA, 'b') ->
          let w' = w * abWb p
           in ( ABState w' SawAB,
                \(ABState dw' _) ->
                  ( ABState (dw' * abWb p) m,
                    (ABParam 0 (dw' * w), Token '\0')
                  )
              )
        _ ->
          ( ABState 0 Rejected,
            const (ABState 0 m, (ABParam 0 0, Token '\0'))
          ),
      dExtract = Diff' $ \(ABState w m) ->
        let y = case m of
              SawA -> w
              SawAB -> w
              _ -> 0
         in (y, (`ABState` m))
    }

-- | Fold a string through a parameterised 'DiffProcess', returning the final
-- output and the total gradient with respect to the parameters.
paramFold ::
  (Add.Additive s, Add.Additive b, Mul.Multiplicative b, Add.Additive p) =>
  DiffProcess s (p, Token) b ->
  p ->
  String ->
  (b, p)
paramFold m p cs =
  let (y, pb) = diffFold m [(p, Token c) | c <- cs]
      grads = pb Mul.one
   in (y, foldl' (Add.+) Add.zero (map fst grads))

-- | Scan a string through a parameterised 'DiffProcess', returning the per-step
-- outputs and the total gradient with respect to the parameters.
paramScan ::
  (Add.Additive s, Add.Additive b, Mul.Multiplicative b, Add.Additive p) =>
  DiffProcess s (p, Token) b ->
  p ->
  String ->
  ([b], p)
paramScan m p cs =
  let (ys, pb) = diffScan m [(p, Token c) | c <- cs]
      grads = pb (replicate (length ys) Mul.one)
   in (ys, foldl' (Add.+) Add.zero (map fst grads))

-- | Gradient bridge: Process gradients for the @"ab" | "a"@ parser.
--
-- On input @"ab"@ with weights @wa = 2@, @wb = 3@ the inside value is @6@ and
-- the gradient is @(3, 2)@ — exactly the first line of
-- 'Circuit.Parser.SemiringProbe.outsideDemo'.  On input @"a"@ the
-- value is @2@ and the gradient is @(1, 0)@.
--
-- >>> processOutsideDemo
-- (6.0,ABParam {abWa = 3.0, abWb = 2.0})
-- (2.0,ABParam {abWa = 1.0, abWb = 0.0})
processOutsideDemo :: IO ()
processOutsideDemo = do
  let p = ABParam 2 3
  print (paramFold abDiffProcess p "ab")
  print (paramFold abDiffProcess p "a")

-- ---------------------------------------------------------------------------
-- Soft-stack pushdown parser
-- ---------------------------------------------------------------------------

-- | Parameters for the soft stack: how much to push on @'a'@, how much to pop
-- on @'b'@.
data StackParam = StackParam
  { pushAmt :: !Double,
    popAmt :: !Double
  }
  deriving (Eq, Show)

instance Add.Additive StackParam where
  zero = StackParam 0 0
  StackParam a b + StackParam a' b' = StackParam (a + a') (b + b')

-- | State of the soft-stack parser: a real-valued stack height.
newtype StackState = StackState {stackHeight :: Double}
  deriving newtype (Eq, Show)

instance Add.Additive StackState where
  zero = StackState 0
  StackState h + StackState h' = StackState (h + h')

-- | Soft-stack parser for the language @a^n b^n@ (n >= 1).
--
-- The machine pushes on @'a'@ and pops on @'b'@.  Extraction returns the
-- negative squared final height, so a perfect parse scores @0@ and deviations
-- are penalised quadratically.  With @push = pop = 1@ the score is maximal for
-- @"ab"@ and @"aabb"@.
softStackProcess :: DiffProcess StackState (StackParam, Token) Double
softStackProcess =
  DiffProcess
    { dInject = Diff' $ \(p, Token c) ->
        let h = case c of
              'a' -> pushAmt p
              'b' -> negate (popAmt p)
              _ -> 0
            pb (StackState dh') =
              let dPush = case c of 'a' -> dh'; _ -> 0
                  dPop = case c of 'b' -> negate dh'; _ -> 0
               in (StackParam dPush dPop, Token '\0')
         in (StackState h, pb),
      dStep = Diff' $ \(StackState h, (p, Token c)) ->
        let dh = case c of
              'a' -> pushAmt p
              'b' -> negate (popAmt p)
              _ -> 0
            h' = h + dh
            pb (StackState dh') =
              let dPush = case c of 'a' -> dh'; _ -> 0
                  dPop = case c of 'b' -> negate dh'; _ -> 0
               in (StackState dh', (StackParam dPush dPop, Token '\0'))
         in (StackState h', pb),
      dExtract = Diff' $ \(StackState h) ->
        let score = negate (h * h)
            pb dy = StackState (negate 2 * h * dy)
         in (score, pb)
    }

-- | Soft-stack smoke test.
--
-- With unit push/pop, @"ab"@ and @"aabb"@ score @0@ (shown as @-0.0@ by
-- floating-point sign rules).  With an unbalanced parameter setting the
-- gradient points toward balance.
--
-- >>> softStackDemo
-- (-0.0,StackParam {pushAmt = 0.0, popAmt = 0.0})
-- (-0.0,StackParam {pushAmt = 0.0, popAmt = 0.0})
-- (-1.0,StackParam {pushAmt = -2.0, popAmt = 2.0})
softStackDemo :: IO ()
softStackDemo = do
  let p1 = StackParam 1 1
  print (paramFold softStackProcess p1 "ab")
  print (paramFold softStackProcess p1 "aabb")
  let p2 = StackParam 2 1
  print (paramFold softStackProcess p2 "ab")
