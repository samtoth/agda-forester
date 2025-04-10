
open import test

data Bool : Set where
    ff : Bool
    tt : Bool


record prod (A : Set) (B : A -> Set) : Set where
  field
    fst : A
    snd :  B fst


module modWithin where
  open import test2 
  test : (A -> B) -> A -> A -> B
  test = S (K K)
  module subsubmod where
    test2 : (A -> B -> B) -> A -> A -> B -> B
    test2 f a a' b = f a (f a' b)

  notInMod : A -> A -> A
  notInMod = K