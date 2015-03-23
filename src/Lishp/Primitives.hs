{-# LANGUAGE RankNTypes #-}
module Lishp.Primitives (primitives) where

import Control.Monad.Error

import Lishp.Types

(|>) = flip ($)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", math (+)),
              ("-", math (-)),
              ("*", math (*)),
              ("/", divOp),
              ("mod", numericBinop mod),
              ("remainder", numericBinop rem)]

math :: (forall a. Num a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
math op           [] = throwError $ NumArgs 2 []
math op singleVal@[_] = throwError $ NumArgs 2 singleVal
math op params        = return $ params |> foldl1 (mathOp op)

mathOp :: (forall a. Num a => a -> a -> a) -> LispVal -> LispVal -> LispVal
mathOp (?) s1 s2 = case (s1, s2) of
    (Integer a, Integer b) -> Integer $ a ? b
    (Integer a, Float b) -> Float $ (fromIntegral a) ? b
    (Float a, Integer b) -> Float $ a ? (fromIntegral b)
    (Float a, Float b) -> Float $ a ? b

divOp :: [LispVal] -> ThrowsError LispVal
divOp           []  = throwError $ NumArgs 2 []
divOp singleVal@[_] = throwError $ NumArgs 2 singleVal
divOp (h:t)         = foldM divOp' h t

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
