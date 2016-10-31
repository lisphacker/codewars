{-# LANGUAGE NamedFieldPuns #-}

module Mix where

import Data.Char
import Data.List
import Data.Ord

{-
split :: [Char] -> [[Char]]
split = filter (\x -> length x > 1) . group . sort . filter isLower

merge :: [[Char]] -> [[Char]] -> [[Char]]
merge [] [] = []
merge (g1w:g1ws) [] = ("1:" ++ g1w):(merge g1ws [])
merge [] (g2w:g2ws) = ("2:" ++ g2w):(merge [] g2ws)
merge (g1w:g1ws) (g2w:g2ws)
  | head g1w == head g2w && length g1w > length g2w = ("1:" ++ g1w):(merge g1ws g2ws)
  | head g1w == head g2w && length g1w < length g2w = ("2:" ++ g2w):(merge g1ws g2ws)
  | head g1w == head g2w && length g1w == length g2w = ("=:" ++ g2w):(merge g1ws g2ws)
  | length g1w > length g2w = ("1:" ++ g1w):(merge g1ws (g2w:g2ws))
  | length g1w < length g2w = ("2:" ++ g2w):(merge (g1w:g1ws) g2ws)
  | head g1w < head g2w = ("1:" ++ g1w):(merge g1ws (g2w:g2ws))
  | otherwise = ("2:" ++ g2w):(merge (g1w:g1ws) g2ws)
        
      
mix :: [Char] -> [Char] -> [Char]
mix s1 s2 = foldl1 (\z x -> z ++ "/" ++ x) $ sortBy cmpfn (merge (split s1) (split s2))
  where cmpfn s1 s2 = if length s1 > length s2 then
                         LT
                      else
                        if length s1 < length s2 then
                          GT
                        else
                          let c1 = head s1
                              c2 = head s2
                          in
                            if c1 == c2 then
                              compare (s1 !! 2) (s2 !! 2)
                            else
                              if c1 == '=' then
                                GT
                              else
                                if c2 == '=' then
                                  LT
                                else
                                  compare c1 c2

-}

data Location = String1 | String2 | Both | Unknown
                deriving (Show)
data CountedAlphabet = CountedAlphabet {
    alphabet :: Char
  , count    :: Int
  }
instance Show CountedAlphabet where
  show (CountedAlphabet a c) = replicate c a

data LocatedAlphabet = LocatedAlphabet {
    location        :: Location
  , countedAlphabet :: CountedAlphabet
  }

instance Show LocatedAlphabet where
  show (LocatedAlphabet l (CountedAlphabet a c)) = "(!" ++ show l ++ "," ++ show a ++ "," ++ show c ++ ")"

split :: [Char] -> [CountedAlphabet]
split = map (\s -> CountedAlphabet (head s) (length s)) . group . sort . filter isLower


test0_1 = "A aaaa bb c"
test0_2 = "& aaa bbb c d"

test1_1 = "my&friend&Paul has heavy hats! &"
test1_2 = "my friend John has many many friends &"
--mix(s1, s2) --> "2:nnnnn/1:aaaa/1:hhh/2:mmm/2:yyy/2:dd/2:ff/2:ii/2:rr/=:ee/=:ss"

test2_1 = "mmmmm m nnnnn y&friend&Paul has heavy hats! &"
test2_2 = "my frie n d Joh n has ma n y ma n y frie n ds n&"
--mix(s1, s2) --> "1:mmmmmm/=:nnnnnn/1:aaaa/1:hhh/2:yyy/2:dd/2:ff/2:ii/2:rr/=:ee/=:ss"

test3_1="Are the kids at home? aaaaa fffff"
test3_2="Yes they are here! aaaaa fffff"
--mix(s1, s2) --> "=:aaaaaa/2:eeeee/=:fffff/1:tt/2:rr/=:hh"
