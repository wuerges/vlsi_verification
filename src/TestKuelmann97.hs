import VerParser
import Algo
import Kuelmann97

import Data.List
import Test.HUnit
import System.Environment
import Equivalence
import Data.Either
import Control.Applicative


filesCorrect = ["BDD", "unit1", "unit10", "unit12", "unit14", "unit16"]
filesWrong   = ["BDD_wrong", "unit2", "unit11", "unit13", "unit15", "unit17"]

makeTest e f = TestCase (do putStrLn $ "Test: " ++ f
                            p1 <- parseVerilog $ "tests/"++f++"/in_1.v"
                            p2 <- parseVerilog $ "tests/"++f++"/in_2.v"
                            case rights [p1, p2] of
                              [r1, r2] -> assertEqual ("Kuelmann97 test: \""
                                            ++ f ++ "\"") e (equiv equivKuelmann97 r1 r2)
                              _ -> assertFailure $ show $ lefts [p1, p2])

tests = TestList $ (map (makeTest True) filesCorrect) ++ (map (makeTest False) filesWrong)


main = runTestTT tests >>= print
