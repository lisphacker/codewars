module Narcissistic where

digits = (map (read :: String -> Integer) . map (\x -> x:[]) . (show :: Integer -> String))

narcissistic :: Integral n => n -> Bool
--narcissistic :: Integer -> Bool
narcissistic n = let i = fromIntegral n
                     d = digits i
                     l = length d
                 in
                   if foldl (+) 0 (map (^l) d) == i then True else False
