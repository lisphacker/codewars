module ExplosiveSum where

import Data.Maybe
import Data.Map.Lazy as Map hiding (foldl)

type MyMap = Map (Int, Int) Int

lkup :: Int -> Int -> MyMap -> Maybe Int
lkup n m mp
    | m == 0    = Just 0
    | n == 0    = Just 1
    | n < 0     = Just 0
    | otherwise = Map.lookup (n, m) mp

buildMap :: Int -> Int -> MyMap -> MyMap
buildMap n m mp = case lkup n m mp of
                    Just v  -> mp
                    Nothing -> let (n1, m1) = (n - m, m)
                                   (n2, m2) = (n, m - 1)
                                   mp2 = buildMap n2 m2 $ buildMap n1 m1 mp
                                   r1  = fromJust $ lkup n1 m1 mp2
                                   r2  = fromJust $ lkup n2 m2 mp2
                               in Map.insert (n, m) (r1 + r2) mp2

esum :: Int -> Int
esum n
    | n < 0     = 0
    | n == 0    = 1
    | n == 1    = 1
    | otherwise = let mp = buildMap n n Map.empty
                  in mp ! (n, n)
                                 

explosiveSum :: Integer -> Integer
explosiveSum = fromIntegral . esum . fromIntegral
