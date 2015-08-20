module Graph where

import Verilog 
import Data.Graph.Inductive
import Control.Arrow
import Debug.Trace

-- Model
type G = Gr () Bool


wireNodes :: Verilog Int -> [Context () Bool]
wireNodes v = [([], n, (), []) | n <- names v]

embedWires :: Verilog Int -> G -> G
embedWires v g = foldr (&) g (wireNodes v)

trues  = repeat True
falses = repeat False

embedF :: Function Int -> G -> G
embedF (Fun op os is) g = case traceShow ("embedF ", op, os, is , g)  op of
       And  -> embedAnd is o g
       Nand -> embedNand is o g
       Or   -> embedOr is o g
       Nor  -> embedNor is o g
       Xor  -> embedXor is o g
       Xnor -> embedXnor is o g
       Buf  -> embedBuf i os g
       Not  -> embedNot i os g
    where [i] = is
          [o] = os

-- TODO
-- or(c, a, b) = and(not c, not a, not b)
embedXor  = error "undefined Xor"
embedXnor = error "undefined Xnor"


negateA :: Adj Bool -> Adj Bool
negateA = map (first not)
negateCtx (is, n, (), os) = (is, n, (), negateA os)

-- Helper Function
negateV :: Int -> G -> G
negateV n g = let (mc, g') = match n g
              in case mc of 
                Just ctx -> negateCtx ctx & g'
                Nothing -> error "could not find vertex in negateV"

-- Should insert only edges
embedBuf :: Int -> [Int] -> G -> G
embedBuf i os g = foldr (\o g -> insEdge (i, o, True) g) g os 

embedNot i os g = foldr (\o g -> insEdge (i, o, True) g) (foldr negateV g os) os 

embedAnd :: [Int] -> Int -> G -> G
embedAnd is o g = foldr (\i g -> insEdge (i, o, True) g) g is

embedNand :: [Int] -> Int -> G -> G
embedNand is o g = embedAnd is o (negateV o g)

embedOr :: [Int] -> Int -> G -> G
embedOr is o g = embedNor is o (negateV o g)

embedNor :: [Int] -> Int -> G -> G
embedNor is o g = foldr (\i g -> insEdge (i, o, False) g) g is

xor :: Bool -> Bool -> Bool
xor True  y = not y
xor False y = y


fixSingleNode :: Int -> G -> G
fixSingleNode n g =  case match n g of
    (Just ctx, g') -> case ctx of
       ([(vi, ni)], n, (), [(vo, no)]) -> insEdge (ni, no, not $ xor vi vo) g'
       _                               -> g
    (Nothing, _)  -> error "Could not match context in fixSingleNode"

fixSingleNodes g = foldr fixSingleNode g (nodes g)

makeGraphV v = fixSingleNodes $ foldr embedF (embedWires v empty) (reverse $ _functions v)

