{-# LANGUAGE TemplateHaskell #-}
module QC_BDD (runTests) where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Graph.Inductive

import BDDGraph
import BDDGraphMonad
import BDDGraphCommon

instance Arbitrary V where
  arbitrary = do
    i <- arbitrary
    r <- arbitrary
    return $ V i r

newtype S_BDD = S_BDD T
  deriving Show


genNode :: Gen Node
genNode = arbitrary `suchThat` (>1)
--do Positive x <- arbitrary
             --return x

instance Arbitrary S_BDD where
  arbitrary = do l <- listOf genNode
                 return $ S_BDD (reserveNodes l startingT)
  shrink  _ = []

-- Ideas for tests
-- 1. Check if there is only 2 sons for every node. or 0.
--

prop_simplest :: S_BDD -> Bool
prop_simplest (S_BDD g) = all (\x -> length x == 0) outs
  where ns = nodes g
        outs = map (out g) ns


newtype I_BDD = I_BDD T
  deriving Show

instance Arbitrary I_BDD where
  arbitrary = do S_BDD x <- arbitrary
                 is <- sublistOf $ [n | n <- nodes x, n > 1]
                 return $ I_BDD x
  shrink  _ = []



newtype G_BDD = G_BDD T
  deriving Show


bddAndMany' repr sons g = withBDD g (bddAndMany repr sons)



withG_G_BDD x =
  let els = [n | (n, V v _) <- labNodes x, v == (-2)]
   in do
     r <- case els of
            [] -> return Nothing
            els' -> Just <$> elements els'

     sons <- sublistOf ([B n
       | (n, V v _) <- labNodes x, v /= (-2)])
       `suchThat` (\l -> length l > 1)

     return $ G_BDD $ bddAndMany' r sons x


tryNegate x =
  let els = [n | (n, V v _) <- labNodes x, v > 1]
   in case els of
        [] -> return x
        els' -> do
          n <- elements els'
          return $ snd $ negateBDD (B n) x

instance Arbitrary G_BDD where
  arbitrary = oneof [ do I_BDD x <- arbitrary
                         withG_G_BDD x
                    , do G_BDD x <- arbitrary
                         withG_G_BDD x
                    , do G_BDD x <- arbitrary
                         G_BDD <$> tryNegate x
                    ]


newtype R_BDD = R_BDD T
  deriving Show

instance Arbitrary R_BDD where
  arbitrary = do G_BDD x <- arbitrary
                 return $ R_BDD $ withBDD x $ reduceAll


prop_input_sons (I_BDD g) = all f ns

  where ns = nodes g
        f n = case context g n of
                (is, 0, V v b, os) -> os == [] && v == (-1) &&  b
                (is, 1, V v b, os) -> os == [] && v == (-1) &&  b
                (is, n, V v b, os) ->
                  (v == (-2) && length os == 0) || (length os == 2)


propManual_input_sons2 (G_BDD g) = prop_input_sons (I_BDD g)
propManual_reduced_bdd (R_BDD g) = prop_input_sons (I_BDD g)

--runTests = $verboseCheckAll
--runTests = $quickCheckAll

return []
runTests = do
  $quickCheckAll
  quickCheckWith stdArgs { maxSuccess = 1 } propManual_input_sons2
  quickCheckWith stdArgs { maxSuccess = 1 } propManual_reduced_bdd


