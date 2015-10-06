module Feynman where

countSquares :: Integer -> Integer
countSquares n = cs n
    where cs 1 = n * n
          cs s = let d = (n - s) + 1
                  in d * d + (cs (s - 1))
