--module BDDGraph (BDD, initialBDD, negateBDD ) where
module BDDGraph where

import BDDGraphCommon
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
import Util

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
--layers = map (map fst) . sortAndGroupBy ginput . filter (\x -> ginput x > 0) . labNodes
layers t = map (map fst) .
  sortAndGroupBy ginput .
    filter (\(n, _) -> outdeg t n > 0) .
      labNodes $ t

sortAndGroupBy p = groupBy (equating p) . sortBy (comparing p)
  where equating p x y = (p x) == (p y)

groupWithSons g = map (map fst) . sortAndGroupBy snd . map (\n -> (n, getSons g n)) . filter (flip gelem g)

moveParents' :: (Node, Node) -> T -> T
moveParents' (a, b) t
  | gelem a t && gelem b t = moveParents a b t
  | otherwise = t

moveParents :: Node -> Node -> T -> T
moveParents top bot t =
  insEdges es $ ([], node_keep, V inp r_keep, os_bot) & t''
  --foldl' (\t e -> insEdge e t) (([], node_keep, V inp r_keep, os_bot) & t'') es
  --(is_top ++ is_bot, node_keep, V inp r_keep, os_bot) & t''
  where
    (Just (is_top, _, V _   r_top, _), t') = match top t
    (Just (is_bot, _, V inp r_bot, os_bot), t'') = match bot t'
    node_keep = min top bot
    r_keep = r_top || r_bot
    es = [(a, node_keep, c) | (c,a) <- is_top ++ is_bot]

 {-
checkReduce1 :: Node -> T -> Maybe (Node, Node)
checkReduce1 n t =
  if gelem n t && outdeg t n > 0 && z == o
     then Just (n, z)
     else Nothing
  where (z, o) = getSons t n
  -}


checkReduce2 :: T -> (Node, Node) -> Bool
checkReduce2 t (n1, n2) =
  gelem n1 t && gelem n2 t && z1 == z2 && o1 == o2
  where (z1, o1) = getSons t n1
        (z2, o2) = getSons t n2

regroup :: [Node] -> [(Node, Node)]
regroup [] = []
regroup [x] = []
regroup (x:xs) = zip (repeat x) xs
