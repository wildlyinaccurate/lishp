module Main where

import Lishp.Interpreter
import System.Environment

main :: IO ()
main = do
         args <- getArgs
         putStrLn (readExpr (args !! 0))
