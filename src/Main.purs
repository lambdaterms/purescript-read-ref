module Main
  ( main
  , modify
  , modify'
  , new
  , read
  , ReadRef
  , write
  , Ref
  )
  where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Ref ∷ Type → Type → Type

foreign import data ReadRef ∷ Type → Type

toReadRef ∷ ∀ a h. Ref h a → ReadRef a
toReadRef = unsafeCoerce

foreign import newRef ∷ ∀ a h. a → Effect (Ref h a)

new ∷ ∀ a b. a → (∀ h. Ref h a → b) → Effect { readRef ∷ ReadRef a, mutator ∷ b }
new val m = do
  ref ← newRef val
  pure { readRef: toReadRef ref, mutator: m ref}

foreign import read ∷ ∀ a. ReadRef a → Effect a

foreign import modify ∷ ∀ a h. Ref h a → (a → a) → Effect a

foreign import modify' ∷ ∀ a b h. Ref h a → (a → { state ∷ a, value ∷ b }) → Effect b

foreign import write ∷ ∀ a h. Ref h a → a → Effect Unit

mutator ∷ ∀ h. Ref h String → Effect _
mutator ref = do
  log "writing to ref"
  write ref "new"

  log "reading from mutator"
  v ← read (toReadRef ref)
  log v
  -- XXX: this won't work
  -- pure ref

main ∷ Effect Unit
main = do
  { readRef, mutator: m } ← new "initial" mutator
  read readRef >>= log
  h ← m
  read readRef >>= log
