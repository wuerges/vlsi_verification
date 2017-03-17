--module BDDGraph (BDD, initialBDD, negateBDD ) where
module BDDGraph where

import Text.Dot
import Debug.Trace
import Data.Graph.Inductive
import Data.Graph.Inductive.Dot
import Control.Monad.State
import Data.Ord
import Data.List
import Data.Maybe
import qualified Data.IntMap as M
import Graph


newtype BDD = B Int
  deriving (Eq, Ord, Show)

data V =  V { input :: Node
            , repr :: Bool }
  deriving Show

type T = Gr V Bool
type BDDOrdering = (Node -> Node -> Ordering)

dottyBDD t = dotty (showBDD t)

labelN (n, v) = [("label", label)] ++ shape
  where label = show n ++ "," ++ show (input v)
        shape = if repr v then [("shape","rectangle")] else []

-- | Converts a graph to a GraphViz format
showBDD g = showDot $ do
  --fglToDot $ gmap (\(is, n, v, os) -> ([], n, (n, input v, repr v), [])) g
  forM_ (labNodes g) $ \(n, v) -> userNode (userNodeId n)  (labelN (n, v))

  forM_ (labEdges g) $
    \(o, d, t) -> edge (userNodeId o) (userNodeId d) (if t then [] else [("style","dotted")])
  mapM_ (same . map userNodeId) (layers g)

type Ctx = Context V Bool

startingT :: T
startingT = mkGraph [(0,V (-1) True), (1, V (-1) True)] []

reserveNodes :: [Node] -> T -> T
reserveNodes ns =
  insNodes $ zip ns (repeat (V (-2) True))

initialBDD :: Node -> T -> T
initialBDD n g = ctx & g'
  where (Just (_, _, v, _), g') = match n g
        ctx = ([], n, V n True, [(True, 1), (False, 0)])

 {-
initialBDD' :: [Node] -> T -> T
initialBDD' ns g = foldr initialBDD g ns
  -}

-- | Exported function
bddOne = B 1
-- | Exported function
bddZero = B 0

inputNode :: Node -> T -> Node
inputNode o g = v
  where Just (V v _) = lab g o

newNode :: T -> Node
newNode = head . newNodes 1

dupNode :: Maybe Node -> Node -> T -> (Node, T)
dupNode repr orig g =
  case repr of
    Nothing -> (z, g1)
    Just x -> (x, insNode (x, V v0 True) g)
 where
   v0 = inputNode orig g

   z = newNode g
   g1 = insNode (z, V v0 False) g

getSons :: T -> Node -> (Node, Node)
getSons g n = case out g n of
                 [(_, l, False), (_, r, True)] -> (l, r)
                 [(_, r, True), (_, l, False)] -> (l, r)
                 x -> error ("// getSons: x was unexpected: " ++ show (n, x) ++ "\n" ++ showBDD g ++ "\n")


newParent :: Maybe Node -> Node -> (Node, Node) -> T -> (BDD, T)
newParent repr orig (l, r) g = (B n', g'')
  where (n', g') = dupNode repr orig g
        g'' = insEdges [(n', l, False), (n', r, True)] g'

-- | Exported function

negateBDD :: BDD -> T -> (BDD, T)
negateBDD (B 0) g = (B 1, g)
negateBDD (B 1) g = (B 0, g)
negateBDD (B n) g = newParent Nothing n (l', r') g''
  where (l, r) = getSons g n
        (B l', g') = negateBDD (B l) g
        (B r', g'') = negateBDD (B r) g'

bddPurge :: BDD -> T -> T
bddPurge (B 0) g = g
bddPurge (B 1) g = g
bddPurge (B n) g
    | null ps = bddPurge' (B l) $ bddPurge' (B r) $ delNode n g
    | otherwise = g
  where ps = inn g n
        (l, r) = getSons g n

bddPurge' :: BDD -> T -> T
bddPurge' (B n) g
    | r = g
    | otherwise = bddPurge (B n) g
  where Just (V _ r) = lab g n

ginput :: (Node, V) -> Node
ginput (_,v) = input v

layers :: T -> [[Node]]
layers = map (map fst) . sortAndGroupBy ginput . filter (\x -> ginput x > 0) . labNodes

rmdups :: (Ord a, Eq a) => [a] -> [a]
rmdups = map head . group . sort

sortAndGroupBy p = groupBy (equating p) . sortBy (comparing p)
  where equating p x y = (p x) == (p y)

groupWithSons g = map (map fst) . sortAndGroupBy snd . map (\n -> (n, getSons g n)) . filter (flip gelem g)

