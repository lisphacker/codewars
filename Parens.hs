module Balanced.Parens where

insertParens :: String -> [String]
insertParens "" = ["()"]

balancedParens :: Int -> [String]
balancedParens 0 = [""]
balancedParens n = let parenStrs = balancedParens (n - 1)
                   in concatMap insertParens parenStrs
