module Codewars.G964.Partition where

import Data.List (sort, nub)
import Text.Printf (printf)
import Control.Monad.State
import Control.Monad.Extra
import qualified Data.IntMap as M

part :: Int -> String
part n = let prods = prod n in
  "Range: " ++ range prods ++ " Average: " ++ average prods ++ " Median: " ++ median prods
  where
    range ps = show $ maximum ps - minimum ps
    avg ps = ((fromIntegral (sum ps)) / (fromIntegral (length ps))) :: Float
    average ps = printf "%.02f" $ avg ps
    median ps = let l = length ps
                in
                  printf "%.02f" $ median' ps l (l `div` 2)
      where
        median' ps l l2
          | l `mod` 2 == 0 = avg [ps !! (l2 - 1), ps !! l2]
          | l `mod` 2 == 1 = fromIntegral (ps !! l2)
                  
    
    
prod :: Int -> [Int]
prod = nub . sort . map (foldl (\z x -> z * x) 1) . enum

enum :: Int -> [[Int]]
enum 0 = []
enum 1 = [[1]]
enum n = [[n]] ++ concatMap (\x -> map (\l -> x:l) (enum (n - x))) [1..(n-1)]


enum' :: Int -> State (M.IntMap [[Int]]) [[Int]]
enum' 0 = return []
enum' 1 = return [[1]]
enum' n = do m <- get
             if M.member n m
               then return (m M.! n)
               else do mconcatMapM enum' recParams
                       m2 <- get
                       modify (M.insert n partitionForN)
                       return partitionForN
                         where partitionForN = [[n]] ++ concatMap (\x -> map (\l -> x:(m2 M.! x))) recParams
                               recParams = map (\x -> n - x) [1..(n-1)]
             

enum'' :: Int -> State (M.IntMap [[Int]]) [[Int]]
enum'' n  = do
  m <- get
  if M.member n m
    then return (m M.! n)
    else do s <- enum'' n
            modify (M.insert n s)
            return s
