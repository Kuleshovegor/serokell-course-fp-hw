{-# LANGUAGE TupleSections #-}
module HW2.T2
  ( distAnnotated
  , distExcept
  , distFun
  , distList
  , distOption
  , distPair
  , distPrioritised
  , distQuad
  , distStream
  , wrapAnnotated
  , wrapExcept
  , wrapFun
  , wrapList
  , wrapOption
  , wrapPair
  , wrapPrioritised
  , wrapQuad
  , wrapStream
  , (++)
  ) where
import HW2.T1 (Annotated (..), Except (..), Fun (..), List (..), Option (..), Pair (..),
               Prioritised (..), Quad (..), Stream (..), mapList)
import Prelude ()
import GHC.Base ( Semigroup((<>)), Monoid(mempty))

distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _)              = None
distOption (_, None)              = None
distOption (Some val1, Some val2) = Some (val1, val2)

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P firstVal1 secondVal1, P firstVal2 secondVal2) =
  P (firstVal1, firstVal2) (secondVal1, secondVal2)

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 a2 a3 a4, Q b1 b2 b3 b4) =
  Q (a1, b1) (a2, b2) (a3, b3) (a4, b4)

distAnnotated
  :: Semigroup e =>
  (Annotated e a, Annotated e b)
  -> Annotated e (a, b)
distAnnotated (value1 :# annotation1, value2 :# annotation2) =
  (value1, value2) :# annotation1<>annotation2

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _)                     = Error e
distExcept (_, Error e)                     = Error e
distExcept (Success value1, Success value2) = Success (value1, value2)

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low val1, Low val2)       = Low (val1, val2)
distPrioritised (Low val1, Medium val2)    = Medium (val1, val2)
distPrioritised (Low val1, High val2)      = High (val1, val2)
distPrioritised (Medium val1, Low val2)    = Medium (val1, val2)
distPrioritised (Medium val1, Medium val2) = Medium (val1, val2)
distPrioritised (Medium val1, High val2)   = High (val1, val2)
distPrioritised (High val1, Low val2)      = High (val1, val2)
distPrioritised (High val1, Medium val2)   = High (val1, val2)
distPrioritised (High val1, High val2)     = High (val1, val2)

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (headS1 :> tailS1, headS2 :> tailS2) =
  (headS1, headS2) :> distStream (tailS1, tailS2)

distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (headL1 :. tailL1, list2)
  = (mapList (headL1,) list2) ++ (distList (tailL1, list2))


(++) :: List a -> List a -> List a
(++) Nil list2                      = list2
(++) (headList1 :. tailList1) list2 = headList1 :. (tailList1 ++ list2)

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F function1, F function2) =
  F (\i -> (function1 i, function2 i))

wrapOption :: a -> Option a
wrapOption value = Some value

wrapPair :: a -> Pair a
wrapPair value = P value value

wrapQuad :: a -> Quad a
wrapQuad value = Q value value value value

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated value = value :# mempty

wrapExcept :: a -> Except e a
wrapExcept value = Success value

wrapPrioritised :: a -> Prioritised a
wrapPrioritised value = Low value

wrapStream :: a -> Stream a
wrapStream value = value :> wrapStream value

wrapList :: a -> List a
wrapList value = value :. Nil

wrapFun :: a -> Fun i a
wrapFun value = F (\_ -> value)
