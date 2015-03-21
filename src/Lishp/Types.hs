module Lishp.Types where

import Control.Monad.Error
import Text.Parsec.Error

data LispVal = Atom String
             | List [LispVal]
             | Integer Integer
             | Float Double
             | String String
             | Bool Bool

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError
