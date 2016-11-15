module Codewars.G964.Mixin where

import Data.List (sort, sortBy, group)
import Data.Char (isLower)
import Debug.Trace

mix :: [Char] -> [Char] -> [Char]
mix s1 s2 = let l1 = clean s1
                l2 = clean s2
            in tail $ mix' l1 l2
 where mix' x1@((v1,c1):p1s) x2@((v2,c2):p2s) = case compare c1 c2 of
         GT -> grpsrt [] c1 x1 x2
         LT -> grpsrt [] c2 x1 x2
         EQ -> grpsrt [] c1 x1 x2

       grpsrt acc l []               []               = shw l acc
       grpsrt acc l x1@((v1,c1):p1s) []               = if c1 == l
                                                        then grpsrt ((1, v1):acc) l p1s []
                                                        else shw l acc ++ mix' x1 []
       grpsrt acc l []               x2@((v2,c2):p2s) = if c2 == l
                                                        then grpsrt ((2, v2):acc) l [] p2s
                                                        else shw l acc ++ mix' [] x2
       grpsrt acc l x1@((v1,c1):p1s) x2@((v2,c2):p2s) = if c1 == l && c2 == l
                                                        then if v1 == v2
                                                             then grpsrt ((0, v1):acc) l p1s p2s
                                                             else if v1 > v2
                                                                  then grpsrt ((1, v1):acc) l p1s (rem v1 x2)
                                                                  else grpsrt ((2, v2):acc) l (rem v2 x1) p2s
                                                        else if c1 == l
                                                             then grpsrt ((1, v1):acc) l p1s (rem v1 x2)
                                                             else if c2 == l
                                                                  then grpsrt ((2, v2):acc) l (rem v2 x1) p2s
                                                                  else shw l acc ++ mix' x1 x2
         where s = "l = " ++ show l ++ ", acc = " ++ show acc
       shw l []          = ""
       shw l ((0, c):ps) = "/=:" ++ replicate l c ++ shw l ps
       shw l ((1, c):ps) = "/1:" ++ replicate l c ++ shw l ps
       shw l ((2, c):ps) = "/2:" ++ replicate l c ++ shw l ps
       
       rem ch [] = []
       rem ch ((v,c):ps) = if v == ch then ps else (v,c):(rem ch ps)

clean :: [Char] -> [(Char, Int)]
clean = sortBy cmpPairs . filter countable . map (\l -> (head l, length l)) . group . sort . filter isLower
  where countable (_, c) = c > 1
        cmpPairs (v1, c1) (v2, c2) = case compare c1 c2 of
          LT -> GT
          GT -> LT
          EQ -> compare v1 v2












-----------------------------------------
s1 = "my&friend&Paul has heavy hats! &"
s2 = "my friend John has many many friends &"
t1 = mix "my&friend&Paul has heavy hats! &" "my friend John has many many friends &"
r1 = "2:nnnnn/1:aaaa/1:hhh/2:mmm/2:yyy/2:dd/2:ff/2:ii/2:rr/=:ee/=:ss"

t2 = mix "mmmmm m nnnnn y&friend&Paul has heavy hats! &" "my frie n d Joh n has ma n y ma n y frie n ds n&"
r2 = "1:mmmmmm/=:nnnnnn/1:aaaa/1:hhh/2:yyy/2:dd/2:ff/2:ii/2:rr/=:ee/=:ss"

t3 = mix "Are the kids at home? aaaaa fffff" "Yes they are here! aaaaa fffff"
r3 = "=:aaaaaa/2:eeeee/=:fffff/1:tt/2:rr/=:hh"
