module Main where

import Lib
import Data.List.Split

data Light = Off | On | Toggle | None deriving (Eq, Show)
type Coordinate = (Int, Int)

data Instruction = Instruction {
  instr :: String,
  start :: Coordinate,
  end   :: Coordinate
} deriving (Show)

(*/) :: Light -> Light -> Light
(*/) On On = On
(*/) On Off = Off
(*/) Off On = On
(*/) Off Off = Off
(*/) Off Toggle = On
(*/) On Toggle = Off
(*/) Off None = Off
(*/) On None = On
(*/) None Off = Off
(*/) None On = On
(*/) Toggle _ = error "Toggling from unknown state"


process_lights [] = []
process_lights (i:is) = zipWith (*/) (gen_grid i) (process_lights is)
gen_grid i = [decode i (a,b) | a <- [0..10],
                               b <- [0..10]]
      where decode i (a,b) = if withinRange (a,b) (start i) (end i) 
                                then get_light i 
                                else None
            get_light i
              | instr i == "on" = On
              | instr i == "off" = Off
              | instr i == "toggle" = Toggle
            withinRange (a,b) (x1,y1) (x2,y2)
              | a >= x1 && a <= x2 && b >= y1 && b <= y2 = True
              | otherwise = False
                
get_instructions instr_lst = [parse instr | instr <- instr_lst]
  where parse = parse_instr . prelim_parse
        prelim_parse lst = splitOn " " lst
        parse_instr [i1,i2,c1,s,c2] = Instruction {instr = i2,
                                                   start = coord_parse c1,
                                                   end = coord_parse c2}
        parse_instr [i1,c1,s,c2] = Instruction {instr = i1,
                                                start = coord_parse c1,
                                                end = coord_parse c2}
        parse_instr s = error (show s)
        coord_parse s = (read n1 :: Int, read n2 :: Int)
          where [n1, n2] = splitOn "," s

num_lights_on = length $ filter (== On) lights
lights = take 1000000 $ repeat On

main :: IO ()
main = do
  f <- readFile "input.txt"
  let lst = splitOn "\n" f
  let allButOne = (length lst) - 1
  let instr_lst = take allButOne lst
  print $ get_instructions instr_lst
  let a = [Off, On, Off, On, On]
  let b = [Off, Off, On, On, Off]
  let c = [None, None, None, None, None]
  print $ zipWith (*/) (zipWith (*/) a c) b
  let x = Instruction{instr = "on", start = (0,0), end = (2,2)}
  print $ (process_lights . get_instructions) instr_lst
