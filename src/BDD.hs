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
import Control.Monad.State

-- | A bdd has a source and a graph
data BDD = Zero | One | B BDD Int BDD
  deriving (Eq, Ord, Show)

-- |  accessor for the zero
--zero (B z v o) = z
--zero x = error $ "Could not access zero of " ++ show x

-- | accessor for the one
--one (B z v o) = o
--one x = error $ "Could not access one of " ++ show x

-- | Creates an initial BDD with a 0 and 1 child for a variable v
initialBDD :: Int -> BDD
initialBDD v = B Zero v One


-- | The State mondad for our BDD memoization
type BDDState a = State (M.IntMap BDD) a

--mcreateBDD :: RG -> Int -> BDDState a


putBDD :: Int -> BDD -> BDDState ()
putBDD i bdd = modify (M.insert i bdd)


getBDD :: Int -> BDDState (Maybe BDD)
getBDD i = M.lookup i <$> get

getBDDSon :: (Int, Bool) -> BDDState (Maybe BDD)
getBDDSon (p, b) | b  =             getBDD p
                 | not b = do mbdd <- getBDD p
                              return $ negateBDD <$> mbdd


recreateBDD :: RG -> Int -> BDDState (Maybe BDD)
recreateBDD g i = do msons <- mapM getBDDSon $ lpre g i
                     if all isJust msons
                     then do let sons = catMaybes msons
                                in if (sum $ map bddSize sons) > 50000
                                      then return Nothing
                                      else let result = foldl1 bddAnd sons
                                           in do putBDD i result
                                                 return $ Just result
                     else return Nothing

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

{-
-- | Creates a BDD for the given vertex
-- | If the vertex is a source, the BDD is simple
-- | if the vertex has many sorces, it must join all the sources
createBDD :: RG -> Int -> BDD
createBDD rg = memoFix (createBDDmemo rg)
-}

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

