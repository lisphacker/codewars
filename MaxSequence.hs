module MaxSequence where
import Data.List

-- Return the greatest subarray sum within the array of integers passed in.
maxSequence :: [Int] -> Int
maxSequence = foldl1 max . map (foldl (+) 0) . foldl1 (++) . map inits . tails


t1 = [-2, 1, -3, 4, -1, 2, 1, -5, 4] :: [Int]
