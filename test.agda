{-# OPTIONS --no-load-primitives #-}
module test where

open import Agda.Primitive
  using (Level ; lzero ; lsuc ; _⊔_)
  renaming (Set to Type ; Setω to Typeω )
  public


id : {A : Type} -> A -> A
id x = x 