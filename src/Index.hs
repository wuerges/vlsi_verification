module Index (IdxState, newIdx, resetIdx, getIdx, getIdxs, runIdx) where

import Verilog
import Data.Map
import Control.Monad
import Control.Monad.State

data V = V { idx :: Map Val Int
           , count :: Int
           , inputs :: Map String Int }

type IdxState = State V

startIdx = V { idx = empty
             , count = 2
             , inputs = empty }
             --fromList [(ValZero, 0), (ValOne, 1)] }

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
getIdx ValZero = return 0
getIdx ValOne = return 1

getIdx (Input n) = do
  V m c i <- get
  case Data.Map.lookup n i of
    Just r -> return r
    Nothing -> do
      put $ V m (c+1) (insert n c i)
      return c

getIdx w = do
  V m c i <- get
  case Data.Map.lookup w m of
    Just r -> return r
    Nothing -> do put $ V (insert w c m) (c+1) i
                  return c

getIdxs :: [Val] -> IdxState [Int]
getIdxs = mapM getIdx

runIdx c = flip evalState startIdx c
