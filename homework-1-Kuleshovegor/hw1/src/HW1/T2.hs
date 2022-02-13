module HW1.T2
  ( N (..)
  , nEven
  , nFromNatural
  , nOdd
  , nToNum
  , ncmp
  , ndiv
  , nmod
  , nmult
  , nplus
  , nsub
  ) where

import GHC.Natural (Natural)

-- | Special type of number.
-- The size of the number depends on the nesting of the structure.
data N
  = Z    -- zero
  | S N  -- not a zero
  deriving Show

-- | Addition for N type.
nplus :: N -> N -> N
nplus Z nSummand              = nSummand
nplus (S nSummand1) nSummand2 = nplus nSummand1 (S nSummand2)

-- | Multiplication for N type.
nmult :: N -> N -> N
nmult _ Z                            = Z
nmult nMultiplicanda (S nMultiplier) = nplus nMultiplicanda (nmult nMultiplicanda nMultiplier)

-- | Subtraction for N type (Nothing if result is negative).
nsub :: N -> N -> Maybe N
nsub nMinuend Z                   = Just nMinuend
nsub Z _                          = Nothing
nsub (S nMinuend) (S nSubtrahend) = nsub nMinuend nSubtrahend

-- | Comparison for N type (Do not derive Ord).
ncmp :: N -> N -> Ordering
ncmp nNum1 nNum2 =
  case nsub nNum1 nNum2 of
    Nothing -> LT
    Just Z  -> EQ
    _       -> GT

-- | Returns N type for a natural number.
nFromNatural :: Natural -> N
nFromNatural 0       = Z
nFromNatural natural = S (nFromNatural (natural - 1))

-- | Returns Num for a N type number.
nToNum :: Num a => N -> a
nToNum Z        = 0
nToNum (S nNum) = 1 + nToNum nNum

-- | Parity checking for N type. Return true if N is even.
nEven :: N -> Bool
nEven Z        = True
nEven (S nNum) = nOdd nNum

-- | Parity checking for N type. Return true if N is odd.
nOdd :: N -> Bool
nOdd Z        = False
nOdd (S nNum) = nEven nNum

-- | Integer division for N type. Throw error if divisor is Z.
ndiv :: N -> N -> N
ndiv _ Z                = error "division by zero"
ndiv Z _                = Z
ndiv nDividend nDivisor = case (nsub nDividend nDivisor) of
  Nothing    -> Z
  Just nDiff -> S(ndiv nDiff nDivisor)

-- | Modulo operation for N type. Throw error if divisor is Z.
nmod :: N -> N -> N
nmod _ Z = error "division by zero"
nmod Z _   = Z
nmod nDividend nDivisor = case (nsub nDividend nDivisor) of
  Nothing    -> nDividend
  Just nDiff -> nmod nDiff nDivisor
