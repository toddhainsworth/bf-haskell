module Token where

data BFToken = MoveLeft
             | MoveRight
             | Increment
             | Decrement
             | Output
             | Input
             | Loop [BFToken]
             deriving Show

