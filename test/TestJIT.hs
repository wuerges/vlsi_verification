module TestJIT where

import VerParser
import Algo
import Kuelmann97

import Data.List
import Test.HUnit
import System.Environment
import Equivalence
import Data.Either
import Control.Applicative
import System.Timeout
import LLVMJIT
import Graph
import Verilog

second = 1000000

data TestFile = TF { fn :: String, tvs :: [([Bool], [Bool])] }

bdd_in1 = TF
  { fn = "tests/BDD/in_1.v"
  , tvs =
    [ ([False, False, False], [False])
    , ([False, False, True ], [True ])
    , ([False, True , False], [False])
    , ([False, True , True ], [True ])
    , ([True,  False, False], [False])
    , ([True,  False, True ], [True ])
    , ([True,  True , False], [True ])
    , ([True,  True , True ], [False])
    ]
  }


--filesWrong   = []
--filesCorrect = ["BDD", "unit1", "unit10", "unit12", "unit14", "unit16"]
--filesWrong   = ["BDD_wrong", "unit2", "unit11", "unit13", "unit15", "unit17"]

makeTest :: (String, ([Bool], [Bool])) -> Test
makeTest (n, (is, os)) = TestLabel n $ TestCase $ do
    p <- parseVerilog n
    case p of
      Right r -> do
        let g = makeGraphV . runIndex $ verilogToInt r
        optmodT <- compileF g
        case optmodT of
          Right mod -> do
            res <- runF g mod is
            case res of
              Right x -> assertEqual
                          ("\n-----------------------\ninputs: " ++ show is ++ " outputs:")
                          os x
              Left l  -> assertFailure $ show l
          Left l  -> assertFailure $ show l
      Left l  -> assertFailure $ show l

makeInputs i = zip (repeat $ fn i) (tvs i)


tests = TestLabel "Testing JIT compilation simulation result" $
  TestList $ map makeTest (concatMap makeInputs [bdd_in1])

