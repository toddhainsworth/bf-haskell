module Parser where

import Text.Parsec
import Text.ParserCombinators.Parsec

data BFToken = MoveLeft
             | MoveRight
             | Increment
             | Decrement
             | Output
             | Input
             | Loop [BFToken]
             deriving Show

parseFromString :: String -> Either ParseError [BFToken]
parseFromString s = parse parseTokens "foo" s

parseTokens :: Parser [BFToken]
parseTokens = many parseToken

parseToken :: Parser BFToken
parseToken = parseMoveLeft
         <|> parseMoveRight
         <|> parseIncrement
         <|> parseDecrement
         <|> parseOutput
         <|> parseInput
         <|> parseLoop

parseMoveLeft
  ,parseMoveRight
  ,parseIncrement
  ,parseDecrement
  ,parseOutput
  ,parseInput
  ,parseLoop :: Parser BFToken

parseMoveLeft = char '<' >> return MoveLeft
parseMoveRight = char '>' >> return MoveRight
parseIncrement = char '+' >> return Increment
parseDecrement = char '-' >> return Decrement
parseOutput = char '.' >> return Output
parseInput = char ',' >> return Input
parseLoop = do
  char '['
  tokens <- many parseToken
  char ']'
  return $ Loop tokens
