module Lishp.Interpreter
    (
      readExpr
    , eval
    , evalString
    , extractValue
    , trapError
    ) where

import Control.Monad
import Control.Monad.Error
import Control.Applicative hiding ((<|>), many, optional)

import Text.ParserCombinators.Parsec hiding (spaces)

import Lishp.Types
import Lishp.Primitives

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser String
escapedChars = do char '\\'
                  x <- oneOf "\\\""
                  return [x]

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ many1 (noneOf "\"\\" ) <|>
                    escapedChars
                char '"'
                return $ String (concat x)

parseNumber :: Parser LispVal
parseNumber =  (Float . read) `fmap` try dec
           <|> (Integer . read) `fmap` try neg
           <|> (Integer . read) `fmap` pos

  where pos = many1 digit
        neg = (:) <$> char '-' <*> pos
        dec = (++) <$> (pos <|> neg) <*> ((:) <$> char '.' <*> pos)

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseAtom
         <|> do char '('
                x <- try parseList
                char ')'
                return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Integer _) = return val
eval val@(Float _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

instance Show LispVal where
    show (String s) = show s
    show (Integer n) = show n
    show (Float n) = show n
    show (Bool b) = if b then "#t" else "#f"
    show (List l) = "(" ++ unwordsList l ++ ")"

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (DivisionByZero)              = "Division by zero!"

instance Show LispError where show = showError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
