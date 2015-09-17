import VerParser
import Algo
import Kuelmann97

import Test.HUnit
import System.Environment
import Equivalence
import Data.Either
import Control.Applicative


filesCorrect = ["BDD", "unit1"
               , "unit11", "unit13"
               , "unit15", "unit17"
               , "unit3", "unit5" --, "unit6_removed"
               , "unit7", "unit9"]

filesWrong = ["BDD_wrong", "unit10"
             , "unit12", "unit14"
             , "unit16", "unit2"
             , "unit4" --, "unit6_removed"
             , "unit8" ]

makeTest e f = TestCase (do p1 <- parseVerilog $ "tests/"++f++"/in_1.v"
                            p2 <- parseVerilog $ "tests/"++f++"/in_2.v"
                            case rights [p1, p2] of
                              [r1, r2] -> assertEqual ("Kuelmann97 test: \""
                                            ++ f ++ "\"") e (equiv equivKuelmann97 r1 r2)
                              _ -> assertFailure $ show $ lefts [p1, p2])

tests = TestList $ (map (makeTest (Just True)) filesCorrect) ++ (map (makeTest Nothing) filesWrong)


main = runTestTT tests >>= print
