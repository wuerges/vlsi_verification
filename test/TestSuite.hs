
import TestJIT
import TestMarshal
import TestGraph
--import TestJuelmann97

import Test.HUnit

sTests = TestJIT.tests ++ TestMarshal.tests ++ TestGraph.tests

main = runTestTT sTests >>= print
