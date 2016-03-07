{-# LANGUAGE RecordWildCards #-}

module Verilog where

import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State

data Function a = Fun { _op  :: Op
                      , _out :: [a]
                      , _in  :: [a] }
    deriving (Ord, Eq, Show)

type IdxState a = State (Int, M.Map String Int) a

nextIdx :: String -> IdxState Int
nextIdx name = do
  (s, idx) <- get
  case M.lookup name idx of
    Just x  -> return x
    Nothing -> do put (s', M.insert name s' idx)
                  return s'
                    where s' = s + 1


runIndex :: IdxState a -> a
runIndex = (flip evalState) (0, M.empty)


--lookIdx :: Index -> String -> Int
--lookIdx i s = fromMaybe (error $ "Should have found " ++ s ++ "in Map!") (M.lookup s $ _map i)

data Op = And
        | Or
        | Buf
        | Xor
        | Xnor
        | Not
        | Nor
        | Nand
    deriving (Ord, Eq, Show)

makeFunction :: Op -> [a] -> Function a
makeFunction Buf ps = Fun Buf (init ps) [last ps]
makeFunction Not ps = Fun Not (init ps) [last ps]
makeFunction op (p:ps) = Fun op [p] ps
makeFunction _ _      = error "Function must have at least 1 input and 1 output wire"


functionToInt :: Function String -> IdxState (Function Int)
functionToInt f@(Fun{..}) = do
  outs  <- mapM nextIdx _out :: IdxState [Int]
  ins   <- mapM nextIdx _in
  return $ f { _out = outs, _in = ins }

data Verilog a = Verilog { _inputs :: [a]
                         , _outputs :: [a]
                         , _functions :: [Function a]
                         }
    deriving Show

names :: Verilog a -> [a]
names v = _inputs v ++ _outputs v ++ concatMap namesFun (_functions v)


verilogToInt :: Verilog String -> IdxState (Verilog Int)
verilogToInt v@(Verilog{..}) = do
  nis <- mapM nextIdx _inputs
  nos <- mapM nextIdx _outputs
  nfs <- mapM functionToInt _functions
  return $ Verilog nis nos nfs


lookIdx = undefined

namesFun :: Function a -> [a]
namesFun f = _out f ++ _in f

emptyVerilog = Verilog { _inputs = []
                       , _outputs = []
                       , _functions = [] }
