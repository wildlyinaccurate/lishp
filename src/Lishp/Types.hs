module Lishp.Types where

import Control.Monad.Except
import Data.IORef

import Text.Parsec.Error

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ExceptT LispError IO

data LispVal = Atom String
             | List [LispVal]
             | Integer Integer
             | Float Double
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }

instance Show LispVal where
    show (Atom s) = s
    show (String s) = show s
    show (Integer n) = show n
    show (Float n) = show n
    show (Bool b) = if b then "#t" else "#f"
    show (List l) = "(" ++ unwordsList l ++ ")"
    show (PrimitiveFunc _) = "<primitive>"
    show (Func { params = args, vararg = varargs, body = body, closure = env }) =
        "(lambda (" ++ unwords (map show args) ++
            (case varargs of
               Nothing -> ""
               Just arg -> " . " ++ arg) ++ ") ...)"

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

instance Show LispError where show = showError

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected [])         = "Expected " ++ show expected
                                       ++ " args; none given"
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (DivisionByZero)              = "Division by zero!"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
