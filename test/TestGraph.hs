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
import Graph
import Verilog

second = 1000000

filesCorrect = ["BDD", "unit1", "unit10", "unit12", "unit14", "unit16"]
filesWrong   = ["BDD_wrong", "unit2", "unit11", "unit13", "unit15", "unit17"]

makeTest f = TestCase (do putStrLn $ " Test: " ++ f
                          p <- parseVerilog $ "tests/"++f++"/in_1.v"
                          case p of
                              Right r -> do
                                let s_inputs  = length ( _inputs  r)
                                let s_outputs = length ( _outputs r)
                                let g = makeGraphV . runIndex $ verilogToInt r
                                assertEqual ("Input  Size test: \"" ++ f ++ "\"") s_inputs  (length $ inputs g)
                                assertEqual ("Output Size test: \"" ++ f ++ "\"") s_outputs (length $ outputs g)
                                --a <- return $ Just $ equiv equivKuelmann97M r1 r2
                              Left l -> assertFailure $ show l)


tests = TestList $ map makeTest (filesCorrect ++ filesWrong)


main = runTestTT tests >>= print
