module Lishp.Types where

import Control.Monad.Error

import Text.Parsec.Error

data LispVal = Atom String
             | List [LispVal]
             | Integer Integer
             | Float Double
             | String String
             | Bool Bool

instance Eq (LispVal) where
  Atom a == Atom b = a == b
  List a == List b = a == b
  Integer a == Integer b = a == b
  Float a == Float b = a == b
  Integer a == Float b = fromIntegral a == b
  Float a == Integer b = a == fromIntegral b
  String a == String b = a == b
  Bool a == Bool b = a == b
  _ == _ = False

type ThrowsError = Either LispError

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | DivisionByZero
               | Default String

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default
