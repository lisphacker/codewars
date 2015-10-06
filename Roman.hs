module Roman where

val 'I' = 1
val 'V' = 5
val 'X' = 10
val 'L' = 50
val 'C' = 100
val 'D' = 500
val 'M' = 1000

solution :: String -> Int
solution (c:[]) = val c
solution (c1:c2:cs) = let v1 = val c1 
                      in
                        if v1 < val c2 then
                            solution (c2:cs) - v1
                        else
                            solution (c2:cs) + v1
