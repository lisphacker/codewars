module Change where

import Data.List

countChange :: Integer -> [Integer] -> Integer
countChange n dens = ccRec n $ sort dens
    where ccRec n dens
              | null dens = 0
              | n < 0 = 0
              | n == 0 = 1
              | n < head dens = 0
              | n == head dens = 1
              | otherwise = foldl (+) 0 $ map (\l -> ccRec (n - head l) l) $ init $ (tails dens)
