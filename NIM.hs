module Main where

import Data.List
import Data.Ord
import Data.Bits
import Data.Maybe

-- | Returns the index and the number picked from a board
chooseMove :: [Int] -> (Int,Int)
chooseMove [] = error "Invalid board"
chooseMove heaps = let x = foldl xor 0 heaps
                   in if x == 0 then
                          if maximum heaps > 1 then
                              error "You will lose :("
                          else
                              fromJust (find (\(i, h) -> h > 0) (zip [0..] heaps))
                      else
                          let sums = map (\t -> (t `xor` x) < t) heaps
                              chosenHeap = fromJust (elemIndex True sums)
                              nbRemove = (heaps !! chosenHeap) - ((heaps !! chosenHeap) `xor` x)
                              heapsTwoMore = foldl f 0 (zip [0..] heaps)
                                  where f z (i, heap) = if (if chosenHeap == i then heap - nbRemove else heap) > 1 then 
                                                            z + 1 
                                                        else 
                                                            z
                          in
                            if heapsTwoMore == 0 then
                                let chosenHeap2 = fromJust (elemIndex (maximum heaps) heaps)
                                    heapsOne = sum $ map (\t -> if t == 1 then 1 else 0) heaps
                                    chosenHeapSize = heaps !! chosenHeap2
                                    nbRemove2 = if chosenHeapSize == 1 then
                                                    1
                                                else
                                                    if odd heapsOne then 
                                                        chosenHeapSize - 1
                                                    else
                                                        chosenHeapSize
                                in
                                  (chosenHeap2, nbRemove2)
                            else
                                (chosenHeap, nbRemove)
                                


b = [4, 2, 5, 1, 6] :: [Int]
heaps = [1, 1, 1, 2] :: [Int]

x = foldl xor 0 heaps
sums = map (\t -> (t `xor` x) < t) heaps
chosenHeap = fromJust (elemIndex True sums)
nbRemove = (heaps !! chosenHeap) - ((heaps !! chosenHeap) `xor` x)
heapsTwoMore = foldl f 0 (zip [0..] heaps)
    where f z (i, heap) = if (if chosenHeap == i then heap - nbRemove else heap) > 1 then z + 1 else z

chosenHeap2 = fromJust (elemIndex (maximum heaps) heaps)
heapsOne = sum $ map (\t -> if t == 1 then 1 else 0) heaps
nbRemove2 = if odd heapsOne then (heaps !! chosenHeap2) - 1 else heaps !! chosenHeap2


