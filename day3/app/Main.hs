module Main where

import Lib
import Data.Set

parse_mapChar c (x,y) 
  | c == '^' = (x,y-1)
  | c == 'v' = (x,y+1)
  | c == '<' = (x-1,y)
  | c == '>' = (x+1,y)

num_houses house_map = (length . fromList) ((0,0) : (get_houses (0,0)house_map))
num_houses_robo house_map = (length . fromList) ((0,0) : houses_santa ++ houses_robo)
  where houses_santa = get_houses (0,0) [house_map !! i | i <- [0,2..l+1]]
        houses_robo = get_houses (0,0)  [house_map !! j | j <- [1,3..l+1]]
        l = length house_map

get_houses cur_cd (c:cs)
  | cs == [] = []
  | otherwise = new_cd : get_houses new_cd cs
  where new_cd = parse_mapChar c cur_cd

main :: IO ()
main = do
  house_map <- readFile "input.txt"
  print $ num_houses house_map
  print $ num_houses_robo house_map
