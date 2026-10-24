module Reexports where

-- should be excluded
open import Agda.Primitive public using (lzero; lsuc; _⊔_; Setω)

-- should remain searchable
open import Agda.Builtin.Nat public using (Nat; zero; suc)
