module LengthLexicographic where

newtype LengthList a = LengthList [a]
    deriving(Show,Eq)

lst (LengthList a) = a

instance Ord a => Ord (LengthList a) where
    compare (LengthList x) (LengthList y) = let lenCmp = compare (length x) (length y) 
                                            in
                                              case lenCmp of
                                                EQ -> compare x y
                                                _  -> lenCmp

x = LengthList [1,2,3,4,5]
y = LengthList [1,2,3,4]
z = LengthList [1,2,3,4,6]
