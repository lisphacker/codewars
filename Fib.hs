module Fib where

import Control.Monad.State
import qualified Data.IntMap as M
import Debug.Trace

fib1 :: Int -> Int
fib1 0 = 0
fib1 1 = 1
fib1 n = (fib1 (n - 1)) + (fib1 (n - 2))

fib2 :: Int -> State (M.IntMap Int) Int
fib2 0 = return 0
fib2 1 = return 1
fib2 n = do m <- get
            if M.member n m
              then return (m M.! n)
              else let (v1, m1) = runState (fib2 (n - 1)) m
                       (v2, m2) = runState (fib2 (n - 2)) m1
                       v = v1 + v2
                   in do put (M.insert n v m2)
                         --traceM $ "n = " ++ show n ++ ", v = " ++ show v
                         return v

fib :: Int -> Int
fib n = evalState (fib2 n) M.empty
{-
fib2 1 = return 1
fib2 n = do
  m <- get
-}
