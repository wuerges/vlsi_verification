module TestEquiv where

import VerParser
import Algo
import System.Environment
import Equivalence
import Data.Either

main :: IO ()
main = do
          [f1, f2] <- getArgs
          p1 <- parseVerilog f1
          p2 <- parseVerilog f2
          case rights [p1, p2] of
            [r1, r2] -> print $ equiv equivKuelmann97 r1 r2
            _ -> error $ show $ lefts [p1, p2]

