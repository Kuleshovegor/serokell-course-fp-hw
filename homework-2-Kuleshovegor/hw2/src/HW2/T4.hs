module HW2.T4
  ( Expr (..)
  , Prim (..)
  , State (..)
  , eval
  , joinState
  , mapState
  , modifyState
  , wrapState
  ) where
import qualified Control.Monad
import HW2.T1 (Annotated (..), mapAnnotated)

data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState mapFunction (S f) = S (mapAnnotated mapFunction . f)

wrapState :: a -> State s a
wrapState value = S (\state -> value :# state)

joinState :: State s (State s a) -> State s a
joinState (S f) = S $ \state ->
  let (S g) :# state1 = f state
      value :# state2 = g state1
      in value :# state2

modifyState :: (s -> s) -> State s ()
modifyState modifyFunction = S $ \state -> () :# (modifyFunction state)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a =
    Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- absF
  | Sgn a        -- signum

data Expr = Val Double | Op (Prim Expr)

instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
  x - y = Op (Sub x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  fromRational x = Val (fromRational x)
  x / y = Op (Div x y)

evalBinary
  :: (Double -> Double -> Prim Double)
  -> Expr
  -> Expr
  -> (Double -> Double -> Double)
  -> State [Prim Double] Double
evalBinary constructor leftArgument rightArgument operator = do
  evalLeftArg <- eval leftArgument
  evalRightArg <- eval rightArgument
  modifyState ((constructor evalLeftArg evalRightArg):)
  return $ operator evalLeftArg evalRightArg

evalUnary
  :: (Double -> Prim Double)
  -> Expr
  -> (Double -> Double)
  -> State [Prim Double] Double
evalUnary constructor argument operator = do
  evalArgument <- eval argument
  modifyState ((constructor evalArgument):)
  return $ operator evalArgument

eval :: Expr -> State [Prim Double] Double
eval (Val value)                 = do return value
eval (Op (Add leftArg rightArg)) = evalBinary Add leftArg rightArg (+)
eval (Op (Sub leftArg rightArg)) = evalBinary Sub leftArg rightArg (-)
eval (Op (Mul leftArg rightArg)) = evalBinary Mul leftArg rightArg (*)
eval (Op (Div leftArg rightArg)) = evalBinary Div leftArg rightArg (/)
eval (Op (Abs argument))         = evalUnary Abs argument (abs)
eval (Op (Sgn argument))         = evalUnary Sgn argument (signum)
