module Interpreter where

import Control.Monad.State

import Token

data BFState = BFState {
  tape :: [Int],
  position :: Int,
  out :: String
} deriving (Show)

defaultState :: BFState
defaultState = BFState (replicate 30000 0) 0 ""

interpret :: [BFToken] -> String
interpret tks = evalState (runCommands $ tks) defaultState

runCommands :: [BFToken] -> State BFState String
runCommands tks = do
  mapM_ runCommand tks
  s <- get
  return $ out s

runCommand :: BFToken -> State BFState ()
runCommand MoveLeft = modify $ \s -> s { position = (position s) - 1 }
runCommand MoveRight = modify $ \s -> s { position = (position s) + 1 }
runCommand Increment = modify $ \s -> s { tape = changeAt (position s) (tape s) (1 +) }
runCommand Decrement = modify $ \s -> s { tape = changeAt (position s) (tape s) (\i -> i - 1) } -- annoying that we can't just use the - function
runCommand Output = modify $ \s -> s { out = (out s) ++ (show $ ((tape s) !! (position s))) }
runCommand Input = undefined
runCommand (Loop tks) = mapM_ runCommand tks

changeAt :: Int -> [Int] -> (Int -> Int) -> [Int]
changeAt pos tape f = h ++ [f newVal] ++ newTail
  where (h, t) = splitAt pos tape
        newVal = if (length t) == 0 then 0 else head t
        newTail = if (length t) == 0 then [] else tail t
