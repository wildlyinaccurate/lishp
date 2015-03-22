module Main where

import Control.Monad
import System.Environment

import Lishp.Interpreter

main :: IO ()
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled

