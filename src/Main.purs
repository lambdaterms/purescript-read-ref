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

foreign import new ∷ ∀ h a. a → Effect {ref ∷ ReadRef a, mutation ∷ RefMutation h a}
foreign import read ∷ ∀ a. ReadRef a → Effect a

newRefMutator ∷ ∀ a b. a → (∀ h. RefMutation h a → b) → Effect {ref ∷ ReadRef a, mutator ∷ b}
newRefMutator val mut = do
  {ref, mutation} ← new val
  pure { ref, mutator: (unsafeCoerce mut) mutation }

mutateRef ∷ ∀ h a. RefMutation h a → (a → a) → Effect a
mutateRef (RefMutation x) f = log "mutating" *> x f

mutatingComponent r = r
  -- XXX: you are not able to return r


main ∷ Effect Unit
main = do
  { ref, mutator } ∷ _ ← newRefMutator 8 mutatingComponent

  traceM mutator

  log "BLBL"
  pure unit



