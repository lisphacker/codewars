module Power where

import Data.List hiding (subsequences)

power :: [a] -> [[a]]
power []     = [[]]
power (x:xs) = let r = power xs
               in map ((++) [x]) r ++ r

i1 = [0,0,0]
i2 = [1,2,3]
