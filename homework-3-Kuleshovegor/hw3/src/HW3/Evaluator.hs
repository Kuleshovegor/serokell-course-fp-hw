{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module HW3.Evaluator
  ( eval
  ) where
import Control.Monad (foldM)
import Data.Foldable (Foldable (toList), find)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import qualified Data.Semigroup
import qualified Data.Semigroup as T
import Data.Sequence (Seq (Empty, (:<|)), fromList, reverse, (><))
import Data.Text as T (length, pack, reverse, strip, toLower, toUpper, unpack)
import HW3.Base (HiError (..), HiExpr (..), HiFun (..), HiValue (..))

data HiValueType =
  HiTypeBool
  | HiTypeNumber
  | HiTypeFun
  | HiTypeNull
  | HiTypeString
  | AnyType
  deriving (Show, Eq)

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval (HiExprValue num) = return $ Right num
eval (HiExprApply expr argsExprs) = do
  evalsValFun  <- eval expr
  evalArgExprs <- evalList argsExprs
  case evalsValFun of
        Left err -> return $ Left err
        Right valFun -> do
          case valFun of
            (HiValueFunction HiFunIf)  -> evalLazy HiFunIf argsExprs
            (HiValueFunction HiFunOr)  -> evalLazy HiFunOr argsExprs
            (HiValueFunction HiFunAnd) -> evalLazy HiFunAnd argsExprs
            _                          -> evalNotLazy valFun argsExprs

evalLazy :: Monad m => HiFun -> [HiExpr] -> m (Either HiError HiValue)
evalLazy HiFunIf [ifExpr, expr1, expr2] = do
  evalRes <- eval ifExpr
  case evalRes of
    Left err                    -> return $ Left err
    Right (HiValueBool boolVal) -> eval $ if boolVal then expr1 else expr2
    _                           -> return $ Left HiErrorInvalidArgument
evalLazy fun [expr1, expr2] = do
  evalRes <- eval expr1
  case evalRes of
    Left err -> return $ Left err
    Right val -> case (fun, val) of
      (HiFunAnd, HiValueBool False) -> return $ Right $ HiValueBool False
      (HiFunAnd, HiValueBool True)  -> eval expr2
      (HiFunOr, HiValueBool True)   -> return $ Right $ HiValueBool True
      (HiFunOr, HiValueBool False)  -> eval expr2
      (_, _)                        -> return $ Left HiErrorInvalidArgument
evalLazy _ _ = return $ Left HiErrorArityMismatch

evalNotLazy :: Monad m => HiValue -> [HiExpr] -> m (Either HiError HiValue)
evalNotLazy valFun argsExpr = do
  argsValEith <- evalList argsExpr
  case argsValEith of
    Left err      -> return $ Left err
    Right argVals -> do
       case valFun of
        HiValueFunction fun -> evalFun fun argVals
        HiValueString str -> do
          resEval <- evalPosOrSlice (unpack str) argVals
          return $ case resEval of
            Right subTxtList -> case (subTxtList, Prelude.length argVals) of
              ([], 1) -> Right $ HiValueNull
              _       -> Right $ HiValueString $ pack subTxtList
            Left err         -> Left err
        HiValueList seqq -> do
          evalRes <- evalPosOrSlice (toList seqq) argVals
          return $ case evalRes of
            Right vals -> case (vals, Prelude.length argVals) of
              ([], 1) -> Right $ HiValueNull
              (_, 1)  -> Right $ head vals
              _       -> Right $ HiValueList $ fromList vals
            Left err   -> Left err
        _                   -> return $ Left HiErrorInvalidFunction

evalList :: Monad m => [HiExpr] -> m (Either HiError [HiValue])
evalList [] = return $ Right []
evalList [expr] = do
  eithEval <- eval expr
  return $ case eithEval of
    Right resVal -> Right [resVal]
    Left err     -> Left err
evalList (expr : tailExprs) = do
   eithEval <- eval expr
   case eithEval of
    Left err -> return $ Left err
    Right resVal -> do
      evalTail <- evalList tailExprs
      return $ case evalTail of
        Left err      -> Left err
        Right tailVal -> Right $ resVal : tailVal

slice :: Int -> Int -> [a] -> [a]
slice from to list = take (to - from) (drop from list)

evalPosOrSlice :: Monad m => [a] -> [HiValue] -> m (Either HiError [a])
evalPosOrSlice list [HiValueNumber ind] = do
  if denominator ind == 1
  then do
    res <- evalPosOrSlice list [HiValueNumber $ toRational ind
      , HiValueNumber $
      if ind == -1
      then toRational $ Prelude.length list
      else ind + 1]
    return $ case res of
      Left err -> Left err
      Right subList ->
        if null subList
        then Right []
        else Right $ subList
  else return $ Left $ HiErrorInvalidArgument
evalPosOrSlice list [from, HiValueNull] =
  evalPosOrSlice list [from, HiValueNumber $ toRational $ Prelude.length list]
evalPosOrSlice list [HiValueNull, to] =
  evalPosOrSlice list [HiValueNumber 0, to]
evalPosOrSlice list [HiValueNumber from, HiValueNumber to] = do
  return $ if denominator from == 1 && denominator to == 1
    then do
      let len = Prelude.length list
      let toIndex = (\ind -> fromIntegral $ if ind >= 0
          then
            ind
          else
            (toInteger len) + ind)
      let fromInt = max (toIndex $ numerator from) 0
      let toInt = min (toIndex $ numerator to) len
      Right $ slice fromInt toInt list
    else Left $ HiErrorInvalidArgument
evalPosOrSlice _ [_, _] = return $ Left HiErrorInvalidArgument
evalPosOrSlice _ _ = return $ Left HiErrorArityMismatch

evalFun :: Monad m => HiFun -> [HiValue] -> m (Either HiError HiValue)
evalFun fun args = do
   case (fun, args) of
    (HiFunDiv, [HiValueNumber _, HiValueNumber 0]) ->
      return $ Left $ HiErrorDivideByZero
    (HiFunDiv, [HiValueNumber x, HiValueNumber y]) ->
      return $ Right $ HiValueNumber $ x/y
    (HiFunDiv, [HiValueString x, HiValueString y]) ->
      return $ Right $ HiValueString $ x <> "/" <> y
    (HiFunDiv, [_, _]) -> return $ Left HiErrorInvalidArgument
    (HiFunMul, [HiValueNumber x, HiValueNumber y]) ->
      return $ Right $ HiValueNumber $ x * y
    (HiFunMul, [HiValueString x, HiValueNumber y]) ->
      return $ if y <= 0 || denominator y /= 1
               then Left $ HiErrorInvalidArgument
               else Right $ HiValueString $ T.stimes (numerator y) x
    (HiFunMul, [HiValueList x, HiValueNumber y]) ->
      return $ if y <= 0 || denominator y /= 1
               then Left $ HiErrorInvalidArgument
               else Right $ HiValueList $ stimes (numerator y) x
    (HiFunMul, [_, _]) -> return $ Left HiErrorInvalidArgument
    (HiFunAdd, [HiValueNumber x, HiValueNumber y]) ->
      return $ Right $ HiValueNumber $ x + y
    (HiFunAdd, [HiValueString x, HiValueString y]) ->
      return $  Right $ HiValueString $ x <> y
    (HiFunAdd, [HiValueList x, HiValueList y]) ->
      return $  Right $ HiValueList $ x >< y
    (HiFunAdd, [_, _]) ->
      return $ Left HiErrorInvalidArgument
    (HiFunSub, [HiValueNumber x, HiValueNumber y]) ->
      return $ Right $ HiValueNumber $ x - y
    (HiFunSub, [_, _]) ->
      return $ Left HiErrorInvalidArgument
    (HiFunNot, [HiValueBool x]) ->
      return $ Right $ HiValueBool $ not x
    (HiFunNot, [_]) ->
      return $ Left HiErrorInvalidArgument
    (HiFunAnd, [HiValueBool x, HiValueBool y]) ->
      return $ Right $ HiValueBool $ x && y
    (HiFunAnd,[_, _]) ->
      return $ Left HiErrorInvalidArgument
    (HiFunOr, [HiValueBool x, HiValueBool y]) ->
      return $  Right $ HiValueBool $ x || y
    (HiFunOr, [_, _]) ->
      return $ Left HiErrorInvalidArgument
    (HiFunLessThan , [x, y]) ->
      return $  Right $ HiValueBool $ x < y
    (HiFunLessThan , _) ->
      return $  Left $ HiErrorArityMismatch
    (HiFunGreaterThan , [x, y]) ->
      return $  Right $ HiValueBool $ x > y
    (HiFunGreaterThan , _) ->
      return $   Left $ HiErrorArityMismatch
    (HiFunEquals , [x, y]) ->
      return $   Right $ HiValueBool $ x == y
    (HiFunEquals , _) ->
      return $ Left $ HiErrorArityMismatch
    (HiFunNotLessThan , [x, y]) ->
      return $ Right $ HiValueBool $ x >= y
    (HiFunNotLessThan , _) ->
      return $ Left $ HiErrorArityMismatch
    (HiFunNotGreaterThan , [x, y]) ->
      return $ Right $ HiValueBool $ x <= y
    (HiFunNotGreaterThan , _) ->
      return $ Left $ HiErrorArityMismatch
    (HiFunNotEquals , [x, y]) ->
      return $ Right $ HiValueBool $ x /= y
    (HiFunNotEquals , _) ->
      return $ Left $ HiErrorArityMismatch
    (HiFunIf, [HiValueBool x, y, z]) ->
      return $ Right (if x then y else z)
    (HiFunIf, [_, _, _]) ->
      return $ Left HiErrorInvalidArgument
    (HiFunLength, [HiValueString s]) ->
      return $ Right $ HiValueNumber $ toRational $ T.length s
    (HiFunLength, [HiValueList   seqq]) ->
      return $ Right $ HiValueNumber $ toRational $ Prelude.length seqq
    (HiFunLength, [_]) ->
      return $ Left HiErrorArityMismatch
    (HiFunToUpper, [HiValueString s]) ->
      return $ Right $ HiValueString $ T.toUpper s
    (HiFunToUpper, [_]) -> return $ Left $ HiErrorInvalidArgument
    (HiFunToLower, [HiValueString s]) ->
      return $ Right $ HiValueString $ T.toLower s
    (HiFunToLower, [_]) ->
      return $ Left $ HiErrorInvalidArgument
    (HiFunTrim, [HiValueString s]) ->
      return $ Right $ HiValueString $ T.strip s
    (HiFunTrim, [_]) ->
      return $ Left $ HiErrorInvalidArgument
    (HiFunReverse, [HiValueString s]) ->
      return $ Right $ HiValueString $ T.reverse s
    (HiFunReverse, [HiValueList seqq]) ->
      return $ Right $ HiValueList $ Data.Sequence.reverse seqq
    (HiFunReverse, [_]) ->
      return $ Left $ HiErrorInvalidArgument
    (HiFunList, xs) ->
      return $ Right $ HiValueList $ fromList xs
    (HiFunRange, [HiValueNumber from, HiValueNumber to]) ->
      return $ Right $ HiValueList $ fromList $ fmap (\e -> HiValueNumber e) ([from..to])
    (HiFunRange, [_, _]) ->
      return $ Left $ HiErrorInvalidArgument
    (HiFunRange, _) ->
      return $ Left $ HiErrorArityMismatch
    (HiFunFold , [_, HiValueList Empty]) ->
      return $ Right $ HiValueNull
    (HiFunFold , [funn, HiValueList (h1 :<| tailSeq)]) -> do
      foldM (\acc now -> case acc of
        Right accVal ->
          eval $ HiExprApply (HiExprValue funn) [HiExprValue accVal, HiExprValue now]
        Left err       ->
          return $ Left err
        ) (Right h1) tailSeq
    (HiFunFold , [_, _]) -> return $ Left $ HiErrorInvalidArgument
    (_ , _) -> return $ Left $ HiErrorArityMismatch
