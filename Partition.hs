module Codewars.G964.Partition where

import Data.List (sort, nub)
import Text.Printf (printf)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.IntMap as M
import Debug.Trace

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
prod = nub . sort . map (foldl (*) 1) . enum

enumSlow :: Int -> [[Int]]
enumSlow 0 = []
enumSlow 1 = [[1]]
enumSlow n = [[n]] ++ concatMap (\x -> map (\l -> x:l) (enumSlow (n - x))) [1..(n-1)]

enum :: Int -> [[Int]]
enum n = evalState (enum' n) (M.fromList [(0, []), (1, [[1]])])

enum' :: Int -> State (M.IntMap [[Int]]) [[Int]]
enum' n = do m <- get
             if M.member n m
               then return (m M.! n)
               else do mapM_ enum' recParams
                       m' <- get
                       modify (M.insert n (partitionForN m'))
                       return (partitionForN m')
                         where partitionForN partMap = [[n]] ++ concatMap (\x -> (map (\l -> x:l) (filter ((>=) x . head) (partMap M.! (n - x))))) recParams
                               recParams = reverse [1..n-1]
