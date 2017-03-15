
import QC_BDD
import QC_Cuts
import TestBDDGraph
--import TestGraph
--import TestJuelmann97

import Test.HUnit

--sTests = TestList [TestGraph.tests]

main = do
  QC_Cuts.runTests
  QC_BDD.runTests
  --print ts
  --r <- runTestTT $ TestBDDGraph.tests
  --print r
