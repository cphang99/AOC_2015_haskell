module Main where

import Lib
import Data.List.Split
import Text.ParserCombinators.Parsec

data Command = 
  Command {
              operator :: String
            , source1 :: String
            , source2 :: String
            , destination :: String
            , value :: Integer
          } deriving Show

parse_NOT :: Parser Command
parse_NOT = do
  string "NOT" 
  char ' '
  source <- many letter
  string " -> "
  destination <- many letter
  return Command {  operator = read("NOT"), 
                    source1 = read(source),
                    source2 = "",
                    destination = read(destination),
                    value = 0
                 }

parse_normal :: Parser Command
parse_normal = do
  source1 <- many letter
  char ' '
  operation <- many upper
  char ' '
  source2 <- many letter
  string " -> "
  destination <- many letter
  return Command { operator = read(operation),
                   source1 = read(source1),
                   source2 = read(source2),
                   destination = read(destination),
                   value = 0
                 }

parse_assign :: Parser Command
parse_assign = do
  source <- many letter
  string " -> "
  destination <- many letter
  return Command { operator = "",
                   source1 = read(source),
                   source2 = ""
                   destination = read(destination),                   value = 0
                 }

parse_val :: Parser Command
parse_val = do
  val <- many digits
  string " -> "
  destination <- many letter
  return Command { operator = ""
                   source1 = 
                    
  

main :: IO ()
main = do
  f <- readFile "input.txt"
  let lst = splitOn "\n" f
  print $ lst
