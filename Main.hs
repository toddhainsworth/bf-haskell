module Main where

import System.Environment (getArgs)

import Parser
import Interpreter

bf :: String -> String
bf s = case parseFromString s of
        Left e -> show e
        Right tks -> interpret tks

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ bf (head args)
