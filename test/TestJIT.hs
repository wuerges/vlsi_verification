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

--filesCorrect = ["BDD"]
--filesWrong   = []
filesCorrect = ["BDD", "unit1", "unit10", "unit12", "unit14", "unit16"]
filesWrong   = ["BDD_wrong", "unit2", "unit11", "unit13", "unit15", "unit17"]

makeTest f = TestCase (do putStrLn $ " Test: " ++ f
                          p <- parseVerilog $ "tests/"++f++"/in_1.v"
                          case p of
                              Right r -> do
                                let s_inputs  = length ( _inputs  r)
                                let s_outputs = length ( _outputs r)
                                let g = makeGraphV . runIndex $ verilogToInt r
                                --mapM_ (print) (contexts g)
                                r <- runJITG g
                                case r of
                                  Right x -> putStrLn "ok"
                                  Left l  -> assertFailure $ show l
                              Left l -> assertFailure $ show l)


tests = TestList $ map makeTest (filesCorrect ++ filesWrong)


main = runTestTT tests >>= print
