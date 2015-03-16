module Lishp.Types where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Integer Integer
             | String String
             | Bool Bool
