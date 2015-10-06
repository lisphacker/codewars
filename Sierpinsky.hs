module Sierpinsky where
import Data.List

sierpinsky :: Integral a => a -> String
sierpinsky n = intercalate "\n" $ map tail $ sier n 2 [" L"]
    where sier 0 _   sl = sl
          sier n len sl = sier (n - 1) (len * 2) (concat [sl, map (\x -> x ++ take (len - (length x)) (repeat ' ') ++ x) sl])
