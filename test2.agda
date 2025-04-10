{-# OPTIONS --no-load-primitives #-}
module test2 where

open import test

variable
  A B C : Type

K : A -> B -> A
K a _ = a

S : (A -> B -> C) -> (A -> B) -> A -> C
S f g a = f a (g a)
