{-# LANGUAGE LambdaCase #-}
module HW2.T1
  ( Annotated (..)
  , Except (..)
  , Fun (..)
  , List (..)
  , Option (..)
  , Pair (..)
  , Prioritised (..)
  , Quad (..)
  , Stream (..)
  , Tree (..)
  , mapAnnotated
  , mapExcept
  , mapFun
  , mapList
  , mapOption
  , mapPair
  , mapPrioritised
  , mapQuad
  , mapStream
  , mapTree
  ) where

import Prelude()

data Option a
  = None
  | Some a

data Pair a = P a a

data Quad a = Q a a a a

data Annotated e a = a :# e
infix 0 :#

data Except e a
  = Error e
  | Success a

data Prioritised a
  = Low a
  | Medium a
  | High a

data Stream a = a :> Stream a
infixr 5 :>

data List a
  = Nil
  | a :. List a
infixr 5 :.

data Fun i a = F (i -> a)

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption f = \case
  None       -> None
  Some value -> Some (f value)

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f = \(P firstValue secondValue) -> P (f firstValue) (f secondValue)

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f = \(Q firstValue secondValue thirdValue fourthValue)
  -> Q (f firstValue) (f secondValue) (f thirdValue) (f fourthValue)

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f = \(value :# annotation) -> (f value) :# annotation

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept f = \case
  Error errorE  -> Error errorE
  Success value -> Success (f value)

mapPrioritised
  :: (a -> b)                          -- ^ map function
  -> (Prioritised a -> Prioritised b)  -- ^ function from prioritised with old value to prioritised with new value
mapPrioritised f = \case
  Low value    -> Low (f value)
  Medium value -> Medium (f value)
  High value   -> High (f value)

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f = \(headS :> tailS) -> f headS :> mapStream f tailS

mapList :: (a -> b) -> (List a -> List b)
mapList f = \case
  Nil            -> Nil
  headL :. tailL -> f headL :. mapList f tailL

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun mapFuntion = \(F fun) -> F (\argument -> mapFuntion (fun argument))

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree f = \case
  Leaf       -> Leaf
  Branch
   leftTree
   value
   rigthTree -> Branch (mapTree f leftTree) (f value) (mapTree f rigthTree)
