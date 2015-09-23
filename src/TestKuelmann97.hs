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

second = 1000000

filesCorrect = ["BDD", "unit1", "unit10", "unit12", "unit14", "unit16"]
filesWrong   = ["BDD_wrong", "unit2", "unit11", "unit13", "unit15", "unit17"]

makeTest e f = TestCase (do putStrLn $ " Test: " ++ f
                            p1 <- parseVerilog $ "tests/"++f++"/in_1.v"
                            p2 <- parseVerilog $ "tests/"++f++"/in_2.v"
                            case rights [p1, p2] of
                              [r1, r2] -> do
                                a <- timeout (120 * second) (return $! equiv equivKuelmann97 r1 r2)
                                case a of
                                  Just r -> assertEqual ("Kuelmann97 test: \"" ++ f ++ "\"") e r
                                  Nothing -> assertFailure $ "Kuelmann97 test: " ++ f ++ " timed out"
                              _ -> assertFailure $ show $ lefts [p1, p2])


tests = TestList $ (map (makeTest False) filesWrong) ++ (map (makeTest True) filesCorrect)


main = runTestTT tests >>= print
