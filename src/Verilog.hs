module Verilog where

import qualified Data.Map as M
import Data.Maybe

data Function a = Fun { _op  :: Op
                      , _out :: [a]
                      , _in  :: [a] }
    deriving (Ord, Eq, Show)

data Index = Index { _seed :: Int
                   , _map   :: M.Map String Int }

emptyIndex = Index 0 M.empty

lookIdx :: Index -> String -> Int
lookIdx i s = fromMaybe (error $ "Should have found " ++ s ++ "in Map!") (M.lookup s $ _map i)

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

functionToInt :: Index -> Function String -> Function Int
functionToInt idx f = f { _out = map (lookIdx idx) $ _out f
                        , _in  = map (lookIdx idx) $ _in f
                        }


data Verilog a = Verilog { _inputs :: [a]
                         , _outputs :: [a]
                         , _functions :: [Function a]
                         }
    deriving Show

names :: Verilog a -> [a]
names v = _inputs v ++ _outputs v ++ concatMap namesFun (_functions v)

attIndex :: Index -> String -> Index
attIndex i@(Index s m) ns = case M.lookup ns m of
                            Just x -> i
                            Nothing -> Index (s+1) (M.insert ns s m)

attIndexV :: Index -> Verilog String -> Index
attIndexV i v = foldl attIndex i $ names v

verilogToInt :: Verilog String -> Index -> Verilog Int
verilogToInt v idx = Verilog nis nos nfs
    where nis = map (lookIdx idx)       $ _inputs    v
          nos = map (lookIdx idx)       $ _outputs   v
          nfs = map (functionToInt idx) $ _functions v

namesFun :: Function a -> [a]
namesFun f = _out f ++ _in f

emptyVerilog = Verilog { _inputs = []
                       , _outputs = []
                       , _functions = [] }
