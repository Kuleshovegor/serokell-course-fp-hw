module HW2.T5
  ( EvaluationError (..)
  , ExceptState (..)
  , eval
  , joinExceptState
  , mapExceptState
  , modifyExceptState
  , throwExceptState
  , wrapExceptState
  ) where
import qualified Control.Monad
import HW2.T1 (Annotated (..), Except (..), mapAnnotated, mapExcept)
import HW2.T2 (wrapExcept)
import HW2.T4 (Expr (..), Prim (..))

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState mapFunction exceptState =
  ES $ \state ->
    mapExcept (\annotated -> mapAnnotated mapFunction annotated) (runES exceptState state)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState value = ES $ \state -> wrapExcept (value :# state)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES f) = ES $ \state1 ->
  case f state1 of
    Success (ES g :# state2) -> g state2
    Error errorE             -> Error errorE

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState modifyFunction =
  ES $ \state -> Success (() :# modifyFunction state)

throwExceptState :: e -> ExceptState e s a
throwExceptState errorE = ES $ \_ -> Error errorE

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero

evalBinary 
  :: (Double -> Double -> Prim Double)
  -> Expr
  -> Expr
  -> (Double -> Double -> Double)
  -> ExceptState EvaluationError [Prim Double] Double
evalBinary constructor leftArgument rightArgument operator = do
  evalLeftArg <- eval leftArgument
  evalRightArg <- eval rightArgument
  modifyExceptState ((constructor evalLeftArg evalRightArg):)
  return $ operator evalLeftArg evalRightArg

evalUnary
  :: (Double -> Prim Double)
  -> Expr
  -> (Double -> Double)
  -> ExceptState EvaluationError [Prim Double] Double
evalUnary constructor argument operator = do
  evalArgument <- eval argument
  modifyExceptState ((constructor evalArgument):)
  return $ operator evalArgument

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val a) = do return a
eval (Op (Add leftArg rightArg)) = evalBinary Add leftArg rightArg (+)
eval (Op (Sub leftArg rightArg)) = evalBinary Sub leftArg rightArg (-)
eval (Op (Mul leftArg rightArg)) = evalBinary Mul leftArg rightArg (*)
eval (Op (Div leftArg rightArg)) = do
  evalLeftArg <- eval leftArg
  evalRightArg <- eval rightArg
  if (evalRightArg == 0)
    then throwExceptState DivideByZero
    else modifyExceptState ((Div evalLeftArg evalRightArg):)
  return $ evalLeftArg / evalRightArg
eval (Op (Abs argument))         = evalUnary Abs argument (abs)
eval (Op (Sgn argument))         = evalUnary Sgn argument (signum)
