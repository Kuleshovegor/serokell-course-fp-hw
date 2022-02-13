module HW2.T3
  ( joinAnnotated
  , joinExcept
  , joinFun
  , joinList
  , joinOption
  ) where
import HW2.T1 (Annotated (..), Except (..), Fun (..), List (..), Option (..))
import Prelude()
import GHC.Base ( Semigroup, (<>))
import HW2.T2 ((++))

joinOption :: Option (Option a) -> Option a
joinOption (Some value) = value
joinOption None         = None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Success value) = value
joinExcept (Error erroeE)  = Error erroeE

joinAnnotated
  :: Semigroup e
  => Annotated e (Annotated e a)
  -> Annotated e a
joinAnnotated ((value :# annotation1) :# annotation2) =
  value :# annotation2 <> annotation1

joinList :: List (List a) -> List a
joinList Nil              = Nil
joinList (headL :. Nil)   = headL
joinList (headL :. tailL) = headL ++ joinList tailL

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\argument -> case f argument of
   F g -> g argument)
