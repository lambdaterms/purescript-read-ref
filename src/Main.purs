module Main
  ( main
  , modify
  , modify'
  , new
  , read
  , ReadRef
  , write
  , WriteRef
  )
  where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Ref ∷ Type → Type

foreign import data ReadRef ∷ Type → Type

foreign import data WriteRef ∷ Type → Type → Type

foreign import newRef ∷ ∀ a. a → Effect (Ref a)

new ∷ ∀ a b. a → (∀ h. WriteRef h a → b) → Effect { readRef ∷ ReadRef a, mutator ∷ b }
new val m = do
  ref ← newRef val
  -- XXX: It could be done without `unsafe` just by returning
  --      the same value twice from FFI
  pure { readRef: unsafeCoerce ref, mutator: m (unsafeCoerce ref)}

foreign import read ∷ ∀ a. ReadRef a → Effect a

foreign import modify ∷ ∀ a h. WriteRef h a → (a → a) → Effect a

foreign import modify' ∷ ∀ a b h. WriteRef h a → (a → { state ∷ a, value ∷ b }) → Effect b

foreign import write ∷ ∀ a h. WriteRef h a → a → Effect Unit

mutator ∷ ∀ h. WriteRef h String → Effect _
mutator ref = do
  log "writing to ref"
  write ref "new"
  -- XXX: this won't work
  -- pure ref

main ∷ Effect Unit
main = do
  { readRef, mutator: m } ← new "initial" mutator
  read readRef >>= log
  h ← m
  read readRef >>= log
