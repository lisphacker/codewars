module RomanNumerals where

solution :: Integer -> String
solution n
    | n >= 1000 = 'M':solution (n - 1000)
    | n >= 900  = 'C':'M':solution (n - 900)
    | n >= 500  = 'D':solution (n - 500)
    | n >= 400  = 'C':'D':solution (n - 400)
    | n >= 100  = 'C':solution (n - 100)
    | n >= 90   = 'X':'C':solution (n - 90)
    | n >= 50   = 'L':solution (n - 50)
    | n >= 40   = 'X':'L':solution (n - 40)
    | n >= 10   = 'X':solution (n - 10)
    | n >= 9    = 'I':'X':solution (n - 9)
    | n >= 5    = 'V':solution (n - 5)
    | n >= 4    = 'I':'V':solution (n - 4)
    | n >= 1    = 'I':solution (n - 1)
    | otherwise = ""
