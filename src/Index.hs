module Index (IdxState, newIdx, resetIdx, getIdx, getIdxs, runIdx) where

import Verilog
import Data.Map
-- import Control.Monad
import Control.Monad.State

data V = V { idx :: Map Val Int
           , old_idx :: Map Val Int
           , count :: Int
           , inputs :: Map String Int }

type IdxState = State V

startIdx = V { idx = empty
             , old_idx = empty
             , count = 2
             , inputs = empty }
             --fromList [(ValZero, 0), (ValOne, 1)] }

resetIdx :: IdxState ()
resetIdx =  do
  V m _ c i <- get
  put $ V empty m c i

newIdx :: IdxState Int
newIdx = do
  V m o c i <- get
  put $ V m o (c+1) i
  return c


getIdx :: Val -> IdxState Int
getIdx ValZero = return 0
getIdx ValOne = return 1

getIdx (Input n) = do
  V m o c i <- get
  case Data.Map.lookup n i of
    Just r -> return r
    Nothing -> do
      put $ V m o (c+1) (insert n c i)
      return c

getIdx w = do
  V m o c i <- get
  case Data.Map.lookup w m of
    Just r -> return r
    Nothing -> do put $ V (insert w c m) o (c+1) i
                  return c

 {-
getOldIdx :: Val -> IdxState Int
getOldIdx w = do
  V m o c i <- get
  case Data.Map.lookup w o of
    Just r -> return r
    Nothing -> do put $ V m o (c+1) i
                  return c
  -}


getIdxs :: [Val] -> IdxState [Int]
getIdxs = mapM getIdx

runIdx :: IdxState a -> (a, Map Val Int, Map Val Int, Map String Int)
runIdx ops = (x, m, o, i)
  where (x, V m o _ i) = flip runState startIdx ops
