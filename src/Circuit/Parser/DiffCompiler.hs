{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Syntax-compiled differentiable parser: 'ParserSyntax' trees become
-- 'DiffProcess' machines with reverse-mode gradients.
--
-- This is the compiled counterpart of the hand-written recognizers in
-- 'Circuit.Parser.ProcessProbe'; the two reproduce the same outside values.
--
-- The machine carries an accumulated semiring weight and a caller-supplied
-- differentiable token-weight function.  Extraction multiplies the accumulated
-- weight by the nullable weight of the current derivative, so the 'DiffProcess'
-- scan gives the inside value and its gradient w.r.t. the parameters.
--
-- === doctests
--
-- >>> processCompilerOutsideDemo
-- (6.0,ABParam {abWa = 3.0, abWb = 2.0})
-- (2.0,ABParam {abWa = 1.0, abWb = 0.0})
module Circuit.Parser.DiffCompiler
  ( -- * Differentiable machine
    DiffState (..),
    compileDiffProcess,

    -- * Demo
    processCompilerOutsideDemo,
  )
where

import Circuit.Diff (Diff, Diff', pattern Diff)
import Circuit.Parser.ProcessProbe (ABParam (..), Token (..), paramFold)
import Circuit.Parser.Syntax
  ( ParserSyntax (..),
    derive,
    nullableValue,
    stringS,
  )
import Circuit.Stats.Diff (DiffProcess (..))
import Control.Applicative (Alternative (empty), (<|>))
import Data.Maybe (fromMaybe)
import NumHask.Algebra.Additive qualified as Add
import NumHask.Algebra.Multiplicative qualified as Mul
import Prelude

-- ---------------------------------------------------------------------------
-- Differentiable machine
-- ---------------------------------------------------------------------------

-- | State of the differentiable machine: an accumulated weight and the current
-- derivative.  The 'Maybe' mode makes the additive instance easy: 'zero' is
-- "no active parse" and addition keeps the left-hand mode.
newtype DiffState r q = DiffState {unDiffState :: (r, Maybe q)}

instance (Add.Additive r) => Add.Additive (DiffState r q) where
  zero = DiffState (Add.zero, Nothing)
  DiffState (w, q) + DiffState (w', q') = DiffState (w Add.+ w', q <|> q')

-- | Total weight of nullable parses of a parser (ignoring result values).
nullableWeight ::
  (Add.Additive r, Mul.Multiplicative r) =>
  ParserSyntax String Char a ->
  r
nullableWeight = maybe Add.zero (const Mul.one) . nullableValue

-- | Compile a weighted parser syntax tree into a 'DiffProcess'.
--
-- The caller supplies a differentiable token-weight function
-- @'Diff' (p, 'Token') r@.  The actual token is selected at runtime, but the
-- gradient w.r.t. the parameter record @p@ is computed by reverse mode through
-- the scan.
compileDiffProcess ::
  forall p r a.
  (Add.Additive r, Mul.Multiplicative r) =>
  (Char -> Diff' (p, Token) r) ->
  ParserSyntax String Char a ->
  DiffProcess (DiffState r (ParserSyntax String Char a)) (p, Token) r
compileDiffProcess weight p0 =
  DiffProcess
    { dInject = injectDiff,
      dStep = stepDiff,
      dExtract = extractDiff
    }
  where
    injectDiff :: Diff' (p, Token) (DiffState r (ParserSyntax String Char a))
    injectDiff = Diff $ \(param, Token c) ->
      let q' = derive c p0
          Diff wDiff = weight c
          (wToken, pw) = wDiff (param, Token c)
       in ( DiffState (wToken, Just q'),
            \dstate ->
              let dw = fst (unDiffState dstate)
                  (dparam, dToken) = pw dw
               in (dparam, dToken)
          )

    stepDiff ::
      Diff'
        (DiffState r (ParserSyntax String Char a), (p, Token))
        (DiffState r (ParserSyntax String Char a))
    stepDiff = Diff $ \(DiffState (w, mq), (param, Token c)) ->
      let q = fromMaybe empty mq
          q'' = derive c q
          Diff wDiff = weight c
          (wToken, pw) = wDiff (param, Token c)
          w' = w Mul.* wToken
       in ( DiffState (w', Just q''),
            \dstate ->
              let dw = fst (unDiffState dstate)
                  dwToW = dw Mul.* wToken
                  dwToToken = dw Mul.* w
                  (dparam, dToken) = pw dwToToken
               in (DiffState (dwToW, mq), (dparam, dToken))
          )

    extractDiff :: Diff' (DiffState r (ParserSyntax String Char a)) r
    extractDiff = Diff $ \(DiffState (w, mq)) ->
      let nw = maybe Add.zero nullableWeight mq
          y = w Mul.* nw
       in (y, \dy -> DiffState (dy Mul.* nw, mq))

-- ---------------------------------------------------------------------------
-- Demo: "ab" | "a" with per-token weights
-- ---------------------------------------------------------------------------

-- | Differentiable weight for the two-token alphabet used in
-- 'Circuit.Parser.SemiringProbe.outsideDemo'.
abWeight :: Char -> Diff' (ABParam, Token) Double
abWeight 'a' =
  Diff $ \(p, _) ->
    ( abWa p,
      \dw -> (ABParam dw 0, Token '\0')
    )
abWeight 'b' =
  Diff $ \(p, _) ->
    ( abWb p,
      \dw -> (ABParam 0 dw, Token '\0')
    )
abWeight _ = Diff $ const (0, const (Add.zero, Token '\0'))

-- | Weighted parser syntax for @\"ab\" | \"a\"@.
abParserSyntax :: ParserSyntax String Char String
abParserSyntax = stringS "ab" <|> stringS "a"

-- | Differentiable recogniser for @\"ab\" | \"a\"@, compiled from syntax.
abDiffProcessCompiled ::
  DiffProcess
    (DiffState Double (ParserSyntax String Char String))
    (ABParam, Token)
    Double
abDiffProcessCompiled = compileDiffProcess abWeight abParserSyntax

-- | Gradient bridge: the compiled 'DiffProcess' reproduces the outside values.
--
-- >>> processCompilerOutsideDemo
-- (6.0,ABParam {abWa = 3.0, abWb = 2.0})
-- (2.0,ABParam {abWa = 1.0, abWb = 0.0})
processCompilerOutsideDemo :: IO ()
processCompilerOutsideDemo = do
  let p = ABParam 2 3
  print (paramFold abDiffProcessCompiled p "ab")
  print (paramFold abDiffProcessCompiled p "a")
