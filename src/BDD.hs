module BDD where

import Verilog
import Graph

import Control.Parallel
import Data.Graph.Inductive
import Control.Arrow
import Debug.Trace
import Data.Maybe
import Data.Function.Memoize
import qualified Data.IntMap as M

-- | A bdd has a source and a graph
data BDD = Zero | One | B BDD Int BDD
  deriving (Eq, Ord, Show)

{-
ype BDDMap = M.IntMap BDD

instance Monad BDDMap where
  return (i, bdd) = M.singleton i bdd

  (Monad m) >>= f = M.
-}

--  accessor for the zero
--zero (B z v o) = z
--zero x = error $ "Could not access zero of " ++ show x

--  accessor for the one
--one (B z v o) = o
--one x = error $ "Could not access one of " ++ show x

-- | Creates an initial BDD with a 0 and 1 child for a variable v
initialBDD :: Int -> BDD
initialBDD v = B Zero v One

-- | Creates a BDD for the given vertex
-- | If the vertex is a source, the BDD is simple
-- | if the vertex has many sorces, it must join all the sources
-- | This is the slow version of the createBDD function
createBDD' :: RG -> Int -> BDD
createBDD' g n = case lpre g n of
    [] -> initialBDD n
    ps -> foldl1 bddAnd (map createBDDe ps)
        where createBDDe (p, b) | b     = createBDD g p
                                | not b = negateBDD $ createBDD g p
-- | calculates all BDDs
--calculateAllBDDs :: RG -> M.IntMap BDD
--calculateAllBDDs g = map (createBDD g) (nodes g)

-- | This complicated version should be completely avoided, since it is probably wrong
calculateAllBDDs :: RG -> M.IntMap BDD
calculateAllBDDs g = cbdds M.empty (nodes g)
  where cbdds m []     = m
        cbdds m (n:ns) = cbdds (fst $ calculateBDD g m n) ns

-- | This complicated version should be completely avoided, since it is probably wrong
calculateBDD :: RG -> M.IntMap BDD -> Int -> (M.IntMap BDD, BDD)
calculateBDD g m n = case lpre g n of
    [] -> dup (m, initialBDD n)
    ps -> dup (m', foldl1 bddAnd bdds)
      where (m', bdds) = foldl createBDDe (m, []) ps
            createBDDe :: (M.IntMap BDD, [BDD]) -> (Int, Bool) -> (M.IntMap BDD, [BDD])
            createBDDe (mi, l) (p, b) | b     = dupl l $ lookupBDD g mi p
                                      | not b = dupl l $ negateBDDm (-p) $ lookupBDD g mi p
            lookupBDD g mi p = case M.lookup p mi of
              Just bdd -> (mi, bdd)
              Nothing  -> calculateBDD g mi p
            negateBDDm k (mi, bdd) = case M.lookup k mi of
              Just bddNeg -> (mi, bddNeg)
              Nothing ->  dup (mi, negateBDD bdd)

  where dup    (m, bdd) = (M.insert n bdd m, bdd)
        dupl l (m, bdd) = (m, bdd:l)

-- | Helper function to be used with createBDD or calculateBDDs
createBDDmemo :: RG -> (Int -> BDD) -> Int -> BDD
createBDDmemo g m n = bddReduce $ case lpre g n of
    [] -> initialBDD n
    ps -> foldl1 bddAnd (map createBDDe ps)
        where createBDDe (p, b) | b  = m p
                                | not b = negateBDD $ m p

createBDDmb_memo :: RG -> M.IntMap BDD -> Int -> Maybe BDD
createBDDmb_memo g m n = case M.lookup n m of
  Just bdd -> Just bdd
  Nothing -> case lpre g n of
    [] -> Just $ initialBDD n
    ps -> if all isJust sons then Just newBDD
                             else Nothing
             where bddSon (p, b) | b  =             M.lookup p m
                                 | not b = negateBDD <$> M.lookup p m
                   sons = map bddSon ps
                   newBDD = foldl1 bddAnd $ catMaybes sons

-- | Creates a BDD for the given vertex
-- | If the vertex is a source, the BDD is simple
-- | if the vertex has many sorces, it must join all the sources
createBDD :: RG -> Int -> BDD
createBDD rg = memoFix (createBDDmemo rg)

-- | Negates the BDD (inverts Zeros and Ones)
negateBDD Zero      = One
negateBDD One       = Zero
negateBDD (B z v o) = B (negateBDD z) v (negateBDD o)


-- | Joist 2 BDDs by a conjunction (and function)
bddAnd :: BDD -> BDD -> BDD

bddAnd Zero _ = Zero
bddAnd _ Zero = Zero
bddAnd One  b = b
bddAnd b  One = b


bddAnd b1@(B z1 v1 o1) b2@(B z2 v2 o2) | v1 > v2 = B (bddReduce $ bddAnd z1 b2) v1 (bddReduce $ bddAnd o1 b2)
                                       | v1 == v2 = B (bddReduce $ bddAnd z1 z2) v1 (bddReduce $ bddAnd o1 o2)
                                       | v1 < v2 = B (bddReduce $ bddAnd z2 b1) v2 (bddReduce $ bddAnd o2 b1)
{-bddAnd b1@(B z1 v1 o1) b2@(B z2 v2 o2) | v1 > v2 = B (r1 `pseq` l1) v1 r1
                                       | v1 == v2 = B (r2 `pseq` l2) v1 r2
                                       | v1 < v2 = B (r3 `pseq` l3) v2 r3
  where l1 = bddAnd z1 b2
        r1 = bddAnd o1 b2
        l2 = bddAnd z1 z2
        r2 = bddAnd o1 o2
        l3 = bddAnd z2 b1
        r3 = bddAnd o2 b1
-}
-- | Reduces a OBDD into a ORBDD
bddReduce :: BDD -> BDD
bddReduce Zero = Zero
bddReduce One = One
bddReduce b@(B z v o) | bddReduce z == bddReduce o    = z
                      | otherwise                    = b

-- | Calculates the size of a BDD
bddSize :: BDD -> Int
bddSize Zero = 1
bddSize One = 1
bddSize (B z _ o) = bddSize z + 1 + bddSize o

