
import TestGraph
--import TestJuelmann97

import Test.HUnit

sTests = TestList [TestGraph.tests]

main = runTestTT sTests >>= print
