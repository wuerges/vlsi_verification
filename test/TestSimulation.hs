import VerParser
import Algo
import Kuelmann97

import Data.List
import Test.HUnit
import System.Environment
import Equivalence
import Data.Either
import Control.Applicative
import Control.Monad
import System.Timeout
import Graph
import Verilog
import Data.Time


filesCorrect = ["BDD", "unit1", "unit10", "unit12", "unit14", "unit16"]
filesWrong   = ["BDD_wrong", "unit2", "unit11", "unit13", "unit15", "unit17"]

makeTest f = TestCase (do putStrLn $ "\n\n Test: " ++ f
                          p <- parseVerilog $ "tests/"++f++"/in_1.v"
                          case p of
                              Right r -> do
                                let s_outputs = length ( _outputs r)
                                let g = makeGraphV . runIndex $ verilogToInt r

                                start <- getCurrentTime
                                os <- replicateM 1000 $ randomSimulateIO g
                                stop <- getCurrentTime
                                print $ diffUTCTime stop start

                                assertEqual ("\nOutput Size test: \"" ++ f ++ "\"") (s_outputs * 1000) (length os)
                                --a <- return $ Just $ equiv equivKuelmann97M r1 r2
                              Left l -> assertFailure $ show l)


tests = TestList $ map makeTest (filesCorrect ++ filesWrong)

main = runTestTT tests >>= print
