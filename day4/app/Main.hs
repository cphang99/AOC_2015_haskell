module Main where

import Lib
import Crypto.Hash
import qualified Data.ByteString.Char8 as C

md5_hash str = show (hash bs :: Digest MD5)
  where bs = C.pack str

adventCoin_finder prefix = head [i | i <- [1..],
                                     let str = prefix ++ show i,
                                     let h = md5_hash str,
                                     take 6 h == "000000"]
                                      
main :: IO ()
main = do
  print $ adventCoin_finder "ckczppom"
