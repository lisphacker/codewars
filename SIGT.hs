module Codewars.Kata.SIGT where
import Prelude hiding (read, reads, readsPrec, Integer, fromIntegral, fromInteger, toInteger)
import Data.Char

stringIntGreaterThan :: String -> String -> Bool

stringIntGreaterThan a b = if a == b then 
                               False 
                           else
                               case ((head a == '-'), (head b == '-')) of
                                 (False, False) -> gt a b
                                 (False, True)  -> True
                                 (True, False)  -> False
                                 (True, True)   -> not $ gt (tail a) (tail b)
    where gt a b = case compare (length a) (length b) of
                     GT -> True
                     LT -> False
                     EQ -> gt2 a b
          gt2 "" "" = False
          gt2 a b   = case compare (head a) (head b) of 
                        GT -> True
                        LT -> False
                        EQ -> gt2 (tail a) (tail b)
          

                             


{-
stringIntGreaterThan a b = (strToInt a) > (strToInt b)
    where strToInt s = if head s == '-' then 
                              -(strToInt2 (tail s) 0)
                          else
                              -(strToInt2 s 0)
          strToInt2 []     acc = acc
          strToInt2 (x:xs) acc = strToInt2 xs (acc * 10 + (ord x) - (ord '0'))
                              
-}
