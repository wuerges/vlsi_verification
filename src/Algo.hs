module Algo where

import Verilog
import Data.Graph.Inductive
import qualified Data.Map as M
import Data.Maybe

-- Model
{-type G = Gr N ()
data N = NFun Function | NWire Wire
    deriving (Ord, Eq, Show)
type IdMap = M.Map N Int
-}


-- Graph Creation

{-
vnodes :: Verilog -> [N]
vnodes v = funs ++ wirs
    where 
        funs = map NFun  $ _functions v
        wirs = map NWire 
                (_inputs v ++ _outputs v ++ _wires v)

idmap :: Verilog -> IdMap
idmap v = M.fromList $ zip (vnodes v) [2..]

lookupId :: IdMap -> N -> Int
lookupId wm w = case w of
        NWire (Value v) -> v
        _ -> fromMaybe 
                (error $ "could not find id for node " ++ show w)
                (M.lookup w wm)


embedNode :: IdMap -> G -> N -> G
embedNode wm g n@(NWire _) = 
    ([], lookupId wm n, n, []) & g
embedNode wm g n@(NFun  f) = 
    (froms, lookupId wm n, n, tos) & g
    where
        froms = [((), lookupId wm $ NWire i) | i <- _in f]
        tos   = [((), lookupId wm $ NWire o)] 
        o = _out f


makeGraph v = foldl (embedNode $ idmap v) empty (vnodes v)

-}
