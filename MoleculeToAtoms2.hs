module MoleculeToAtoms where

import Text.Parsec.Char
import Text.Parsec.Combinator hiding (count)
import Text.Parsec.Prim
import Text.Parsec.Text

count :: Parser [Char]
count = many1 digit

readCount :: String -> String
readCount input = case parse count "M2A" input of
                    Left err  -> "No match: " ++ show err
                    Right val -> "Found value: " ++ show val
