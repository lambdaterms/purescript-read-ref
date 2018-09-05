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

class RefMutator

newtype RefMutation (s ∷ Symbol) a = RefMutation ((a → a) → Effect a)

-- | Create a read-only reference containing specified value,
-- | and a mutating function
foreign import new ∷ ∀ s a. SProxy s → a → Effect {ref ∷ ReadRef a, mutation ∷ RefMutation s a}

-- | Read the current value of a reference
foreign import read ∷ ∀ a. ReadRef a → Effect a


newRefMutator ∷ ∀ s a b. SProxy s → a → (RefMutator ⇒ RefMutation s a → b) → Effect {ref ∷ ReadRef a, mutator ∷ b}
newRefMutator s val mut = do
  {ref, mutation} ← new s val
  pure { ref, mutator: (coerceMutator mut) mutation }

  where
    coerceMutator = unsafeCoerce1

foreign import unsafeCoerce1 ∷ ∀ a. (RefMutator ⇒ a) → a

mutateRef ∷ ∀ s a. RefMutator ⇒ RefMutation s a → (a → a) → Effect a
mutateRef (RefMutation x) f = log "mutating" *> x f

-- logEight ∷ (RefMutation "TEST" Int) → Effect Unit
logEight (RefMutation refMutation1) (RefMutation refMutation2) = do
  x ← refMutation1 (const 8)
  log "log"
  log (show x)
  pure unit


main ∷ Effect Unit
main = do
  { ref, mutator } ← newRefMutator (SProxy ∷ SProxy "TEST") 8 logEight
  -- mutator
  { ref, mutator } ← newRefMutator (SProxy ∷ SProxy "TEST") "STRING" mutator
  log "BLBL"


