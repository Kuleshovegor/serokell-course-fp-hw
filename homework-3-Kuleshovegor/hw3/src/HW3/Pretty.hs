module HW3.Pretty
  ( prettyValue
  ) where

import HW3.Base (HiFun (..),
                 HiValue (..))

import Data.ByteString (intercalate)
import Data.Foldable (toList)
import Data.Ratio (numerator)
import Data.Scientific (FPFormat (Fixed), Scientific (base10Exponent), formatScientific,
                        fromRationalRepetendUnlimited)
import GHC.Real (denominator)
import Prettyprinter (Doc, Pretty (pretty), comma, encloseSep)
import Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueList seqq)
  = encloseSep (pretty "[") (pretty " ]") comma $ toList (fmap (\e -> (pretty " ") <> prettyValue e) seqq)
prettyValue (HiValueString txt)                   = pretty $ show txt
prettyValue (HiValueNull)                         = pretty "null"
prettyValue (HiValueBool True)                    = pretty "true"
prettyValue (HiValueBool False)                   = pretty "false"
prettyValue (HiValueFunction HiFunDiv)            = pretty "div"
prettyValue (HiValueFunction HiFunMul)            = pretty "mul"
prettyValue (HiValueFunction HiFunAdd)            = pretty "add"
prettyValue (HiValueFunction HiFunSub)            = pretty "sub"
prettyValue (HiValueFunction HiFunNot)            = pretty "not"
prettyValue (HiValueFunction HiFunAnd)            = pretty "and"
prettyValue (HiValueFunction HiFunOr)             = pretty "or"
prettyValue (HiValueFunction HiFunLessThan)       = pretty "less-than"
prettyValue (HiValueFunction HiFunGreaterThan)    = pretty "greater-than"
prettyValue (HiValueFunction HiFunEquals)         = pretty "equals"
prettyValue (HiValueFunction HiFunNotLessThan)    = pretty "not-less-than"
prettyValue (HiValueFunction HiFunNotGreaterThan) = pretty "not-greater-than"
prettyValue (HiValueFunction HiFunNotEquals)      = pretty "not-equals"
prettyValue (HiValueFunction HiFunIf)             = pretty "if"
prettyValue (HiValueFunction HiFunLength)         = pretty "length"
prettyValue (HiValueFunction HiFunToUpper)        = pretty "to-upper"
prettyValue (HiValueFunction HiFunToLower)        = pretty "to-lower"
prettyValue (HiValueFunction HiFunReverse)        = pretty "reverse"
prettyValue (HiValueFunction HiFunTrim)           = pretty "trim"
prettyValue (HiValueFunction HiFunList)           = pretty "list"
prettyValue (HiValueFunction HiFunFold)           = pretty "fold"
prettyValue (HiValueFunction HiFunRange)          = pretty "range"
prettyValue (HiValueNumber ratio)
  = case fromRationalRepetendUnlimited ratio of
    (scient, Nothing ) -> pretty $ formatScientific Fixed (Just $ abs (base10Exponent scient)) scient
    (_, Just _) -> case quotRem (numerator ratio) (denominator ratio) of
      (0, _) -> pretty $ show (numerator ratio) <> "/" <> show (denominator ratio)
      (num, rem) -> pretty $ show num <> (if rem > 0 then " + " else " - ") <> (show $ abs rem) <> "/" <> show (denominator ratio)
