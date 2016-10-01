module BDDGraph (NV) where

import Data.Graph.Inductive
import Data.Graph.Inductive.Query.DFS (reachable)
import Control.Monad.State

instance Monoid BDD where
  mempty = bddOne
  mappend = bddAnd

data NV = I Node | Zero | One
  deriving (Eq, Ord, Show)


type G = (Gr NV Bool)
type Ctx = Context NV Bool
data BDD = BDD G Node

type GST = State G

runGST :: G -> GST a -> a
runGST g c = evalState c g

adjustRange :: (Node, Node) -> Node -> Node
adjustRange (_, r) n = n + r

adjustNodeRange :: (Node, Node) -> G -> G
adjustNodeRange r =
  gmap (\(ctx@(is, n, v, os)) ->
      case v of
        (I x) -> (is, adjustRange r n, (I x), os)
        _     -> ctx)

contexts :: G -> [Ctx]
contexts g = map (context g) (nodes g)

merge :: G -> G -> G
merge g1 g2 = foldr (&) g1 (contexts g2)

bundle :: Node -> G -> GST Node
bundle n g2 = do
  g1 <- get
  let g2' = adjustNodeRange (nodeRange g1) $ subgraph (reachable n g2) g2
  put (merge g1 g2)
  return $ adjustRange (nodeRange g1) n



bddOne  = BDD (mkGraph [(1, One)] []) (-1)
bddZero = BDD (mkGraph [(0, Zero)] []) (-1)

initialBDD :: Int -> BDD
initialBDD v = BDD g nv
    where nv = v + 2
          g = mkGraph [(0, Zero), (1, One), (nv, I v)]
                      [(nv, 0, False), (nv, 1, True)]


negateBDD :: BDD -> BDD
negateBDD (BDD g v) = BDD (nmap negateNV g) v
  where
    negateNV Zero = One
    negateNV One = Zero
    negateNV (I x) = (I x)


bddAnd :: BDD -> BDD -> BDD
bddAnd = undefined
  {-bddAnd b1@(g1, n1) b2@(g2, n2)
  | nv1 > nv2  = ([is1,

  where (ctx1@(is1, _, nv1,os1), g1') =  match n1 g1
        (ctx2@(is2, _, nv2,os2), g2') =  match n2 g2

-}

bddReduce :: BDD -> BDD
bddReduce = undefined


bddSize :: BDD -> Int
bddSize (BDD g _) = noNodes g
