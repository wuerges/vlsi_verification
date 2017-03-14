{-# LANGUAGE TemplateHaskell #-}
module QC_BDD (runTests) where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Graph.Inductive

import BDDGraph

instance Arbitrary V where
  arbitrary = do
    i <- arbitrary
    r <- arbitrary
    return $ V i r

newtype S_BDD = S_BDD T
  deriving Show


genNode :: Gen Node
genNode = do Positive x <- arbitrary
             return x

instance Arbitrary S_BDD where
  arbitrary = do l <- listOf genNode
                 return $ S_BDD (reserveNodes l startingG)

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
                 -- TODO ^ this is a problem, node must be > 1
                 return $ I_BDD (initialBDD' is x)



prop_input_sons (I_BDD g) = all f ns
  where ns = nodes g
        f n = case context g n of
                (is, 0, V v b, os) -> os == [] && v == (-1) &&  b
                (is, 1, V v b, os) -> os == [] && v == (-1) &&  b
                (is, n, V v b, os) ->
                  (v == (-2) && length os == 0) || (length os == 2)





return []
--runTests = $verboseCheckAll
runTests = $quickCheckAll