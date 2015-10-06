module CamelCase where

import Data.Char
import Data.List

toCamelCase :: String -> String
toCamelCase s = foldl (++) "" (capStrList 0 (split s))
    where capStrList _ []     = []
          capStrList n (w:ws) = let w2 = if null w || (n == 0 && isLower (head w)) then
                                             w
                                         else
                                             capitalize w
                                in
                                  w2:(capStrList (n + 1) ws)
              where
                capitalize []     = []
                capitalize (c:cs) = (toUpper c):cs
          split [] = []
          split s  = let (l, r) = break (\c -> c == '-' || c == '_') s
                     in l:(if null r then [] else split (tail r))

t1 = "the-stealth-warrior"
t2 = "The_Stealth_Warrior"
t3 = ""
