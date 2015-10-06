module LeastCommonMultiple where
import Prelude hiding (lcm)
import Data.List

primeFactors n = map (\l -> (head l, length l)) $ group $ reverse $ primeFactors2 n [] [2..n]
    where divisibleBy f x = x == (x `div` f) * f
          primeFactors2 _ pfs []     = pfs
          primeFactors2 n pfs (f:fs) = 
              if divisibleBy f n then
                  primeFactors2 (n `div` f) (f:pfs) (f:fs)
              else
                  primeFactors2 n pfs (fs \\ (take (length fs) (filter (\x -> x `mod` f == 0) [2..n])))
       
lcm :: Integral a => [a] -> a                                               
lcm xs = if any (== 0) xs then 
                     0
                 else
                     (foldl (\z x -> z * pow (snd x) (fst x)) 1 . 
                      foldr (\x z -> if null z then [x] else if fst x == (fst . head) z then z else x:z) [] . 
                      sortBy cmp . 
                      concatMap primeFactors) xs
         where cmp p1 p2 = case compare (fst p1) (fst p2) of
                             LT -> LT
                             GT -> GT
                             EQ -> case compare (snd p1) (snd p2) of
                                     LT -> LT
                                     GT -> GT
                                     EQ -> EQ
               pow 1 x = x
               pow p x = x * pow (p - 1) x

                           
