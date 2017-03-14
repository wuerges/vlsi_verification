

import QC_BDD
import TestBDDGraph
--import TestGraph
--import TestJuelmann97

import Test.HUnit

--sTests = TestList [TestGraph.tests]

main = do
  QC_BDD.runTests
  --print ts
  --r <- runTestTT $ TestBDDGraph.tests
  --print r
