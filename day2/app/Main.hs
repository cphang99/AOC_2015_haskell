module Main where

import Lib
import Data.List.Split

areas dim_list formula = [formula dims | s <- dim_list,
                                         let dims = parse_dim s]
  where parse_dim s = [read d :: Int | d <- (splitOn "x" s)]

total_area dim_list = foldr (+) 0 (areas dim_list formula)
  where formula [l,w,h] = (2*l*w) + (2*w*h) + (2*h*l) + (slack l w h)
        slack l w h = minimum [l*w, w*h, h*l]

total_ribbon dim_list = foldr (+) 0 (areas dim_list formula)
  where formula [l,w,h] = (l*w*h) + (smallest_perim l w h)
        smallest_perim l w h = (minimum [l+w, w+h, l+h]) * 2

main :: IO ()
main = do
  s <- readFile "input.txt"
  let l = splitOn "\n" s
  print $ (total_area . drop 1 . reverse) l
  print $ (total_ribbon . drop 1 . reverse) l
