module MoleculeToAtoms where

import Data.Char
import Data.List

data Token = OpenBracket Char
           | CloseBracket Char
           | Element String
           | Count String
           | Invalid | Test0 | Test1 | Test2 | Test3 | Test4 | Test5 | Test6 | Test7 | Test8
             deriving Show
unpack (Element s) = s
unpack (Count n)   = n

instance Eq Token where
    --OpenBracket c1 == OpenBracket c2   = c1 == c2
    --CloseBracket c1 == CloseBracket c2 = c1 == c2
    --(Element e1) == (Element e2)       = e1 == e2
    --(Count c1) == (Count c2)           = c1 == c2
    Invalid == Invalid                 = True
    _ == _                             = False


tokenize :: String -> [Token]
tokenize [] = []
tokenize s@(f:fs)
    | isDigit f      = let (token, rest) = span isDigit s in (Count token):(tokenize rest)
    | isUpper f      = let (token, rest) = span isLower fs in (Element (f:token)):(tokenize rest)
    | f `elem` "({[" = (OpenBracket f):(tokenize fs)
    | f == ')'       = (CloseBracket '('):(tokenize fs)
    | f == '}'       = (CloseBracket '{'):(tokenize fs)
    | f == ']'       = (CloseBracket '['):(tokenize fs)
    | otherwise      = Invalid:(tokenize fs)

parse :: Char -> [Token] -> [Token] -> ([Token], [Token])
parse b acc []                 = if b == ' ' then (acc, []) else ([Invalid], [])
parse b acc (e@(Element _):ts) = parse b (acc ++ [e]) ts
parse b acc ((Count n):ts)     = parse b (init acc ++ replicate (read n) (last acc)) ts
parse b acc ((OpenBracket c):ts)   = let (parsed, rest) = parse c [] ts
                                 in if null rest then 
                                        (acc ++ parsed, [])
                                    else
                                        case head rest of
                                          Count n -> parse b (acc ++ (concat . replicate (read n)) parsed) (tail rest)
                                          _       -> parse b (acc ++ parsed) rest
parse b acc ((CloseBracket c):ts)  = if b == c then (acc, ts) else ([Invalid], [])
parse b acc (Invalid:ts)       = ([Invalid], [])

parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula = let expanded = (fst . parse ' ' [] . tokenize) formula
                        in if Invalid `elem` expanded then
                               Left "Not a valid molecule"
                           else
                               Right ((map (\x -> (head x, length x)) . group . sort . map unpack) expanded)








tokens1 = tokenize "H"
tokens2 = tokenize "O2"
tokens3 = tokenize "H2O"
tokens4 = tokenize "Mg(OH)2"
tokens5 = tokenize "K4[ON(SO3)2]2"
formula = "K4[ON(SO3)2]2"
