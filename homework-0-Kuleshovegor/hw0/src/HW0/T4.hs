module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function (fix)
import GHC.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' f = fix (\rec list -> case list of
  []       -> []
  (x : xs) -> f x : rec xs
  )

fac :: Natural -> Natural
fac = fix (\rec num -> if num <= 1 then 1 else num * rec (num - 1))

fib :: Natural -> Natural
fib = fix (\rec now next num -> if num == 0 then now else rec next (now + next) (num - 1)) 0 1
