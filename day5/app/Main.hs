module Main where

import Lib
import Data.List.Split
import qualified Data.Set as Set

isVowel l = if elem l vowels then True else False
  where vowels = ['a','e','i','o','u']

isBanned s = if elem s banned then True else False
  where banned = ["ab", "cd", "pq", "xy"]

num_nice_strings nice_func str_list = length [s | str <- str_list,
                                                  let s = nice_func str,
                                                  s /= Nothing] 
nice_string :: String -> Maybe String
nice_string str
  | contains_vowels str && consec_letters str && no_banned_str str = Just str
  | otherwise = Nothing
  where contains_vowels str = if length [l | l <- str,
                                             isVowel l] >= 3 then True else False 
        consec_letters str = if length [ls | ls <- divvy 2 1 str,
                                             ls !! 0 == ls !! 1] >0 then True else False
        no_banned_str str = if length [ls | ls <- divvy 2 1 str,
                                            isBanned ls] == 0 then True else False

nice_string_2 :: String -> Maybe String
nice_string_2 str
  | con_reps str && contains_pair str = Just str
  | otherwise = Nothing

con_reps str = if length (filter (hasReps) triplets) > 0 then True else False
  where hasReps [a,b,c]
          | a == c = True
          | otherwise = False
        triplets = [l | l <- divvy 3 1 str]
                                          
contains_pair str = if length pairs /= length set_pairs then True else False
  where pairs = filter_pairs [l | l <- divvy 2 1 str]
        filter_pairs [] = []
        filter_pairs (l:[]) = [l]
        filter_pairs (l:lst) = l : (if l == (head lst)
                                     then (filter_pairs . drop 1) lst
                                     else filter_pairs lst)
        set_pairs = Set.fromList pairs

main :: IO ()
main = do
  f <- readFile "input.txt"
  let str_list = splitOn "\n" f
  let dummy_str= "xxyxx"
  print $ num_nice_strings nice_string str_list
  print $ num_nice_strings nice_string_2 str_list
