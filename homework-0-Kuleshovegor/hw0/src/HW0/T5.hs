module HW0.T5
  ( Nat
  , nFromNatural
  , nToNum
  , nmult
  , nplus
  , ns
  , nz
  ) where

import Numeric.Natural (Natural)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ x = x

ns :: Nat a -> Nat a
ns nat f x = f (nat f x)

nplus, nmult :: Nat a -> Nat a -> Nat a
nplus nat1 nat2 f x = nat1 f (nat2 f x)
nmult nat1 nat2 f = nat1 (nat2 f)

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns $ nFromNatural $ n - 1

nToNum :: Num a => Nat a -> a
nToNum nat = nat (1 + ) 0
