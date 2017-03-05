module Index (IdxState, newIdx, resetIdx, getIdx, getInputs, runIdx) where

import Verilog
import Data.Map
import Control.Monad
import Control.Monad.State

data V = V { idx :: Map Val Int
           , count :: Int
           , inputs :: Map Val Int }

type IdxState = State V

startIdx = V { idx = empty
             , count = 2
             , inputs = fromList [(ValZero, 0), (ValOne, 1)] }

resetIdx :: IdxState ()
resetIdx =  do
    V m c i <- get
    put $ V empty c i

newIdx :: IdxState Int
newIdx = do
  V m c i <- get
  put $ V m (c+1) i
  return c

getIdx :: Val -> IdxState Int
getIdx n = do
  V m c i <- get
  case Data.Map.lookup n i of
    Just r -> return r
    Nothing -> case Data.Map.lookup n m of
                 Just r -> return r
                 Nothing -> do
                   put $ V (insert n c m) (c+1) i
                   return c

getInput :: String -> IdxState Int
getInput n = do
  V m c i <- get
  case Data.Map.lookup (Wire n) i of
    Just r -> return r
    Nothing -> do
      put $ V m (c+1) (insert (Wire n) c i)
      return c

getInputs :: [String] -> IdxState [Int]
getInputs = mapM getInput

runIdx c = flip evalState startIdx c
