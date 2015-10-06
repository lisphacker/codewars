module Fibonacci where

import Data.Map.Lazy as Map

fibonacci :: Int -> Integer
fibonacci n = (buildMap n (Map.insert 0 0 $ Map.insert 1 1 $ Map.empty)) ! n
    where buildMap n mp = case Map.lookup n mp of
                            Just f  -> mp
                            Nothing -> let mp2 = buildMap (n - 2) $ buildMap (n - 1) mp
                                       in Map.insert n ((mp2 ! (n - 1)) + (mp2 ! (n - 2))) mp2
