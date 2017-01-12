module Main where

import Lib

get_floor str = foldr (+) 0 (process_lisp str)
  where process_lisp str = [inter_brackets i | i <- str]
        inter_brackets i
          | i == '(' = 1
          | i == ')' = -1

get_basement str = head [i | i <- [1..(length str)],
                             let s = take i str,
                             let floor = get_floor s,
                             floor == (-1)]

main :: IO ()
main = do
  str <- readFile "input.txt"
  print $ get_floor str
  print $ get_basement str
