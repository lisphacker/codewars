module LargestProduct where
import Data.Char
import Data.List

greatestProduct :: String -> Int
greatestProduct = maximum . map (foldl1 (*) . map digitToInt) . filter (\x -> length x >= 5) . map (take 5) . tails

s1 = "11111"
s2 = "55493"
s3 = "123834539327238239583"
