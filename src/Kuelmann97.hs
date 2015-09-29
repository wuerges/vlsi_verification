module Kuelmann97 where

import Equivalence
import Graph
import BDD

import Control.Monad.State
import Data.List hiding (union)
import Data.Graph.Inductive
import Debug.Trace
import qualified Data.Map as M
import qualified Data.IntMap as I

-- | Checks Equivalence of circuits based on Kuelmann97
equivKuelmann97 :: Checker
equivKuelmann97 g1 g2 os1 os2 = checkExits result os1 os2
    where g = g1 `union` g2
          --(_, _, result) = until (checkStop os1 os2) kuelmannStep (M.empty, is0, g)
          (_, _, _, result) = until (checkStop os1 os2) kuelmannStep2 (m0, I.empty, is0, g)
          m0 = M.empty

          --is0 = bfsn (inputs g) g
          --is0 = topsort g
          is0 = mybfs g

equivKuelmann97M :: Checker
equivKuelmann97M g1 g2 os1 os2 = trace (showGraph g') $ checkExits g' os1 os2
  where g = g1 `union` g2
        todo = mybfs g
        g' = snd . snd $ evalState (runStateT (mapM_ kuelmannStep3 todo) (M.empty, g)) I.empty



-- | Checks if both outputs are mapped to the same node in rg
checkExits :: RG -> [Int] -> [Int] -> Bool
checkExits rg os1 os2 = --traceShow (os1, os2) $
  all (\(o1, o2) -> success $ checkPair rg o1 o2) (zip os1 os2)

-- | Checks if a pair in the exit is equivalent or not
checkPair rg o1 o2 = --trace ("o1: " ++ show o1 ++ " o2:" ++ show o2 ++ " result: " ++ show  result) $
  case result  of
    [only] -> Just True
    _      -> Nothing
  where result = filter (\(n, v) -> elem o1 v && elem o2 v ) (labNodes rg)

-- | Checks if analysis should stop
checkStop :: [Int] -> [Int] -> (M.Map BDD Int, I.IntMap BDD, [Int], RG) -> Bool
checkStop _ _ (_, _, [], _) = True
checkStop os1 os2 (_, _, _, g)  = checkExits g os1 os2


maybeLookup :: Ord k => Maybe k -> M.Map k v -> Maybe v
maybeLookup mk m = case mk of
                   Just k -> M.lookup k m
                   Nothing -> Nothing

-- | Performs one step of the iteration
            --createBDDmb_memo :: RG -> M.IntMap BDD -> Int -> Maybe (M.IntMap BDD, BDD)

kuelmannStep :: (M.Map BDD Int, I.IntMap BDD, [Int], RG) -> (M.Map BDD Int, I.IntMap BDD, [Int], RG)
kuelmannStep (m, bdds, [], g) = (M.empty, I.empty, [] , empty)
kuelmannStep (m, bdds, (i:is), g) =
  if gelem i g  then
                --trace ("is: " ++ show (bddSize <$> mbdd) ++  " -> "++ show (length is) ++ " -> " ++ show (take 5 is) )
                (m', bdds', is, g')
                else (m, bdds, is, g)
  where
        mbdd  = createBDDmb_memo g bdds i
        bdds' = case mbdd of
                  Just bdd -> if bddSize bdd < 5000 then I.insert i bdd bdds else bdds
                  Nothing -> bdds
        (m', g') = case mbdd of
                     Just bdd -> case M.lookup bdd m of
                                   Just c -> (m, if c == i then g else removeUnreach (outputs g) (mergeNodes g i c))
                                   Nothing -> (M.insert bdd i m, g)
                     Nothing -> (m, g)



kuelmannStep2 :: (M.Map BDD Int, I.IntMap BDD, [Int], RG) -> (M.Map BDD Int, I.IntMap BDD, [Int], RG)
kuelmannStep2 (m, bdds, [], g) = (M.empty, I.empty, [] , empty)
kuelmannStep2 (m, bdds, (i:is), g) =
  if gelem i g  then
                trace ("is: " ++ show (mbdd) ++  " -> "++ show (length is) ++ " -> " ++ show (length $ nodes g) )
                (m', bdds'', is, g')
                else (m, bdds, is, g)
  where
        mbdd  = createBDDmb_memo g bdds i
        bdds' = case mbdd of
                  Just bdd -> if bddSize bdd < 5000 then I.insert i bdd bdds else bdds
                  Nothing -> bdds

        (m', bdds'', g') = case mbdd of
                     Just bdd -> case M.lookup bdd m of
                                   Just c -> if c == i then (m, bdds', g) else mergedTriple
                                                where
                                                  mergedG = trace ("Merged " ++ show (c, i) ++ "!!") $ removeUnreach (outputs g) (mergeNodes2 g i c)
                                                  mergedBDD = initialBDD c
                                                  mergedTriple = (M.insert mergedBDD c m, bdds', mergedG)
                                                  --mergedTriple = (M.insert mergedBDD i m, I.insert c mergedBDD $ I.insert i mergedBDD bdds', mergedG)
                                   Nothing -> (M.insert bdd i m, bdds', g)
                     Nothing -> (m, bdds', g)

type KuelmannState a = StateT (M.Map BDD Int, RG) (State (I.IntMap BDD)) a


getGraph :: KuelmannState RG
getGraph = snd <$> get

checkExists :: BDD -> KuelmannState (Maybe Int)
checkExists bdd = do m <- fst <$> get
                     return $ M.lookup bdd m

putGraph :: RG -> KuelmannState ()
putGraph g = do (m, _) <- get
                put (m, g)

updateNode :: BDD -> Int -> KuelmannState ()
updateNode bdd i = do (m, g) <- get
                      put (M.insert bdd i m, g)

deleteNode :: BDD -> KuelmannState ()
deleteNode bdd = do (m, g) <- get
                    put (M.delete bdd m, g)


-- | Monadic version of a step in the kuelmann algorithm
-- | The problem with this version is that it excludes the possibility of a group of 3 nodes being equivalent to each other, since it will only mark
-- | the first 2 as equivalent, merge the next ones and doom the rest of the process
kuelmannStep3 :: Int -> KuelmannState ()
kuelmannStep3 i = do
  g <- getGraph
  if not $ gelem i g
     then return ()
     else do mbdd <- lift (recreateBDD g i)
             case mbdd of
               Nothing -> return ()
               Just bdd -> do mc <- checkExists bdd
                              case mc of
                                Nothing -> trace ("\n\n//new " ++ show i ++ " -- > " ++ show bdd) updateNode bdd i
                                Just c -> let g' = removeUnreach (outputs g) (mergeNodes2 g i c)
                                          in do newBDDm <- lift $ recreateBDD g' i
                                                putGraph (trace ("\n\n//merged " ++ show i ++ " + " ++  show c ++ " --> " ++ show bdd ++ " --> " ++ show newBDDm ++ "\n" ++ showGraph g') g')
                                                case newBDDm of
                                                  Just newBDD -> do deleteNode bdd
                                                                    updateNode newBDD i
                                                                    lift (putBDD i newBDD)
                                                  Nothing -> return ()






-- | Removes vertices that won't reach any output from the Graph
removeUnreach :: [Int] -> RG -> RG
removeUnreach os g = g' --trace ("// " ++ show os ++ " " ++ show (dfs os $ grev g) ++ "\n// before \n" ++ showGraph g ++ "\n// after \n" ++ showGraph g' ) g'
  where g' = subgraph (dfs os $ grev g) g

-- | Merges 2 nodes in the graph. The first one is mantained, the second one is removed
-- | and all its sucessors are moved to the first one.
mergeNodes :: RG -> Int -> Int -> RG
mergeNodes g n1 n2 = case match n2 g of
  (Just ctx2, g') -> case match n1 g' of
     (Just ctx1, g'') -> mergeCtxs ctx1 ctx2 & g''
     _ -> g'
  _ -> g
  where mergeCtxs (is1, _, nv1, os1) (_, _, nv2, os2) = --c3 --trace (unlines $ map (("// " ++) . show) [c1, c2, c3]) $ c3
              (is1, n1, nub([n2] ++ nv1 ++ nv2), nub (os1 ++ os2))
            --where c3 =

mergeNodes2 :: RG -> Int -> Int -> RG
mergeNodes2 g n1 n2 = case match n2 g of
        (Just ctx2, g') -> case match n1 g' of
              (Just ctx1, g'') -> mergeCtxs ctx1 ctx2 & g''
              _ -> g'
        _ -> g
    where mergeCtxs (is1, _, nv1, os1) (_, _, nv2, os2) = --c3 --trace (unlines $ map (("// " ++) . show) [c1, c2, c3]) $ c3
              (is1, n1, nub([n2] ++ nv1 ++ nv2), nub (os1 ++ os2))
            --where c3 =
