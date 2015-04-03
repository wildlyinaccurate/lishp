{-# LANGUAGE RankNTypes #-}
module Lishp.Primitives (primitives) where

import Control.Monad.Error

import Lishp.Types

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericOp (+)),
              ("-", numericOp (-)),
              ("*", numericOp (*)),
              ("/", pdiv),
              ("=", peq),
              ("<", numericBoolBinop (<)),
              ("<=", numericBoolBinop (<=)),
              (">", numericBoolBinop (>)),
              (">=", numericBoolBinop (>=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("mod", integralBinop mod),
              ("remainder", integralBinop rem)]

-- Wrapper for variadic numerical operations
numericOp :: (forall a. (Num a) => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
numericOp op params = if length params < 2
                      then throwError $ NumArgs 2 params
                      else return $ foldl1 (numericBinop op) params

-- Polymorphic binary numerical operations
numericBinop :: (forall a. (Num a) => a -> a -> a) -> LispVal -> LispVal -> LispVal
numericBinop op s1 s2 = case (s1, s2) of
    (Integer a, Integer b) -> Integer $ a `op` b
    (Integer a, Float b) -> Float $ (fromIntegral a) `op` b
    (Float a, Integer b) -> Float $ a `op` (fromIntegral b)
    (Float a, Float b) -> Float $ a `op` b

peq :: [LispVal] -> ThrowsError LispVal
peq []   = throwError $ NumArgs 1 []
peq args = return $ Bool $ and . zipWith (==) args $ drop 1 args

-- Wrapper for polymorphic division
pdiv :: [LispVal] -> ThrowsError LispVal
pdiv []    = throwError $ NumArgs 2 []
pdiv (h:t) = if length t == 0
             then throwError $ NumArgs 2 [h]
             else foldM divBinop h t

-- Polymorphic division
divBinop :: LispVal -> LispVal -> ThrowsError LispVal
divBinop s1 s2
    | s2 == Integer 0 || s2 == Float 0 = throwError $ DivisionByZero
    | otherwise = case (s1, s2) of
        (Integer a, Integer b) -> return $ Integer $ quot a b
        (Integer a, Float b) -> return $ Float $ (fromIntegral a) / b
        (Float a, Integer b) -> return $ Float $ a / (fromIntegral b)
        (Float a, Float b) -> return $ Float $ a / b
        (_, _) -> throwError $ TypeMismatch "number" $ List [s1, s2]

-- Wrapper for binary integral operations
integralBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
integralBinop op           []  = throwError $ NumArgs 2 []
integralBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
integralBinop op params        = mapM unpackIntegral params >>= return . Integer . foldl1 op

-- Integral unpacking and error handling
unpackIntegral :: LispVal -> ThrowsError Integer
unpackIntegral (Integer n) = return n
unpackIntegral (String n) = let parsed = reads n in
                              if null parsed
                                then throwError $ TypeMismatch "integer" $ String n
                                else return $ fst $ parsed !! 0
unpackIntegral (List [n]) = unpackIntegral n
unpackIntegral notNum     = throwError $ TypeMismatch "integer" notNum

-- Numeric unpacking and error handling. Cooerces Integer to Float.
unpackNumber :: LispVal -> ThrowsError Double
unpackNumber (Float n) = return n
unpackNumber (Integer n) = return $ fromIntegral n
unpackNumber (String n) = let parsed = reads n in
                              if null parsed
                                then throwError $ TypeMismatch "number" $ String n
                                else return $ fst $ parsed !! 0
unpackNumber (List [n]) = unpackNumber n
unpackNumber notNum     = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- Wrapper for binary boolean operations
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do
                               left <- unpacker $ args !! 0
                               right <- unpacker $ args !! 1
                               return $ Bool $ left `op` right

numericBoolBinop = boolBinop unpackNumber
boolBoolBinop = boolBinop unpackBool
