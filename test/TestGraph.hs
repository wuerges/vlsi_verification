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
import TestBase

second = 1000000

makeTest f = TestCase (do putStrLn $ "\n-----------------------\n Test: " ++ f
                          p <- parseVerilog $ f
                          case p of
                              Right r -> do
                                let s_inputs  = length ( _inputs  r)
                                let s_outputs = length ( _outputs r)
                                let g = makeGraphV . runIndex $ verilogToInt r
                                --putStrLn $ compileGraph g
                                assertEqual ("Input  Size test: \"" ++ f ++ "\"") s_inputs  (length $ inputs g)
                                assertEqual ("Output Size test: \"" ++ f ++ "\"") s_outputs (length $ outputs g)
                                --a <- return $ Just $ equiv equivKuelmann97M r1 r2
                              Left l -> assertFailure $ show l)





tests = TestList $ map makeTest fileNames


main = runTestTT tests >>= print
