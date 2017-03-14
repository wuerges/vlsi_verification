

import QT_BDDGraph
import TestBDDGraph
--import TestGraph
--import TestJuelmann97

import Test.HUnit

--sTests = TestList [TestGraph.tests]

main = do
  r <- runTestTT $ TestBDDGraph.tests
  print r
