module TestGraph where

import TestBase

import VerParser
import Algo
import Kuelmann97

import Data.List
import Test.HUnit
import System.Environment
import Equivalence
import Data.Either
import Control.Monad.State
import Control.Applicative
import System.Timeout
import LLVMJIT
import Graph
import Verilog

import qualified Data.Map as M
import qualified Data.Set as S


swap (a, b) = (b, a)

reverseMap = M.fromList . (map swap) . M.toList

--diff a b = S.fromList a `S.difference` S.fromList b

diff2 a b = (S.difference sa sb `S.union` S.difference sb sa) `S.difference` s_at_0
  where sa = S.fromList a
        sb = S.fromList b
        --s_at_0 = S.fromList ["1'b0", "n52110"] -- tests/unit11/in_2.v
        s_at_0 = S.fromList ["n52110"] -- tests/unit11/in_2.v





makeTest f =
  TestLabel f $
    TestCase  $
      do p <- parseVerilog $ f
         case p of
           Left l -> assertFailure $ show l
           Right r -> do
             let s_inputs  = length r_inputs
                 s_outputs = length r_outputs
                 r_inputs  = _inputs r
                 r_outputs = _outputs r
                 g_inputs  = map (rm M.!) (inputs g)
                 g_outputs = map (rm M.!) (outputs g)
                 rm        = reverseMap idx
                 din       = diff2 g_inputs r_inputs
                 dout      = diff2 g_outputs r_outputs


                 (vi, idx) = runIndex $ do v <- verilogToInt r
                                           (_, i) <- get
                                           return (v, i)
                 g = makeGraphV vi

             assertEqual ("Diff Inputs: " ++ show din ++ " Diff Outputs: " ++ show dout)  (S.size din, S.size dout) (0, 0)





tests = TestLabel "Testing length of the inputs and outputs of the graphs" $
  TestList $ map makeTest fileNames


