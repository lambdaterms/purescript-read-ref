module Main where

import Prelude

import Control.Apply ((*>))
import Control.Bind (bind, pure)
import Data.Symbol (SProxy(..))
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Class.Console (log)
import Unsafe.Coerce (unsafeCoerce)

foreign import data ReadRef ∷ Type → Type

newtype RefMutation h a = RefMutation ((a → a) → Effect a)

-- | Create a read-only reference containing specified value,
-- | and a mutating function
foreign import new ∷ ∀ h a. a → Effect {ref ∷ ReadRef a, mutation ∷ RefMutation h a}

-- | Read the current value of a reference
foreign import read ∷ ∀ a. ReadRef a → Effect a

newRefMutator ∷ ∀ a b. a → (∀ h. RefMutation h a → b) → Effect {ref ∷ ReadRef a, mutator ∷ b}
newRefMutator val mut = do
  {ref, mutation} ← new val
  pure { ref, mutator: (unsafeCoerce mut) mutation }

-- foreign import coerceMutator ∷ ∀ a. (∀ h. RefMutator h ⇒ a) → a

mutateRef ∷ ∀ h a. RefMutation h a → (a → a) → Effect a
mutateRef (RefMutation x) f = log "mutating" *> x f

-- logEight ∷ (RefMutation "TEST" Int) → Effect Unit
logEight (RefMutation refMutation1) = do -- (RefMutation rM2) = do -- (RefMutation refMutation2) = do
  x ← refMutation1 (const 8)
  log "log"
  log (show x)
  pure unit

-- logEight ∷ (RefMutation "TEST" Int) → Effect Unit
badLogEight (RefMutation refMutation1) (RefMutation rM2) = do -- (RefMutation refMutation2) = do
  x ← refMutation1 (const 8)
  log "log"
  log (show x)
  pure unit


main ∷ Effect Unit
main = do
  { ref, mutator } ∷ _ ← newRefMutator 8 logEight
  mutator
  { ref: ref2, mutator: mutator2 } ∷ _ ← newRefMutator 8 badLogEight
  -- { ref, mutator: m } ← newRefMutator "STRING" mutator2
  -- m
  log "BLBL"
  pure unit



