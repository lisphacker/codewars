module Codewars.Parentheses where

validParentheses :: String -> Bool
validParentheses = validate 0
    where validate depth []     = depth == 0
          validate depth (b:bs) = if depth < 0 then 
                                      False 
                                  else 
                                      case b of
                                        '(' -> validate (depth + 1) bs
                                        ')' -> validate (depth - 1) bs
