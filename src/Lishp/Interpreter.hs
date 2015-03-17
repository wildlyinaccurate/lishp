module Lishp.Interpreter (readExpr, eval) where

import Lishp.Types
import Lishp.Primitives
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Applicative hiding ((<|>), many, optional)

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

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Integer _) = val
eval val@(Float _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

instance Show LispVal where
    show (String s) = show s
    show (Integer n) = show n
    show (Float n) = show n
    show (Bool b) = if b then "#t" else "#f"
    show (List l) = "(" ++ unwords (map show l) ++ ")"
