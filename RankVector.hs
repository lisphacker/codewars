module RankVector where
import Data.List

ranks :: (Eq a, Ord a) => [a] -> [Int]
ranks l = map getRank l
        where getRank x = case lookup x rs of
                            Nothing -> -1
                            Just r  -> r
              rs = zip ((reverse . sort) l) [1..]

{-
ranks l = map (\x -> getRank rs x) l
        where getRank []     _ = -1
              getRank (r:rs) x = if fst r == x then snd r else getRank rs x
              rs = zip ((reverse . sort) l) [1..]
--}
