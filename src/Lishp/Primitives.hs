module Lishp.Primitives (primitives) where

import Control.Monad.Error

import Lishp.Types

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", divOp),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

divOp :: [LispVal] -> ThrowsError LispVal
divOp a@[_] = throwError $ NumArgs 2 a
divOp (h:t) = foldM divOp' h t

divOp' :: LispVal -> LispVal -> ThrowsError LispVal
divOp' s1 s2
    | s2 == Integer 0 || s2 == Float 0 = throwError $ DivisionByZero
    | otherwise = return $ case (s1, s2) of
        (Integer a, Integer b) -> Integer $ quot a b
        (Integer a, Float b) -> Float $ (fromIntegral a) / b
        (Float a, Integer b) -> Float $ a / (fromIntegral b)
        (Float a, Float b) -> Float $ a / b

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Integer . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Integer n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "integer" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "integer" notNum
