module RPN where

import Data.Char

calc :: String -> Double
calc s = (rpn [] . words) s
    where readDbl = read :: String -> Double
          rpn :: [Double] -> [String] -> Double
          rpn []    []     = 0
          rpn stack []     = head stack
          rpn stack (t:ts) = if (isDigit . head) t then
                                 rpn ((readDbl t):stack) ts
                             else
                                 let p2 = head stack
                                     p1 = (head . tail) stack
                                     rs = (tail . tail) stack
                                 in case head t of
                                      '+' -> rpn ((p1 + p2):rs) ts
                                      '-' -> rpn ((p1 - p2):rs) ts
                                      '*' -> rpn ((p1 * p2):rs) ts
                                      '/' -> rpn ((p1 / p2):rs) ts
                                      _   -> error "Unknown operator"
