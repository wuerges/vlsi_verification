module TestEquiv where

import VerParser
import Algo
import System.Environment
import Equivalence
{-
main :: IO ()
main = do
          [f1, f2] <- getArgs
          p1 <- parseVerilog f1
          p2 <- parseVerilog f2
          case (p1, p2) of
            (Right r1, Right r2) -> print $ equiv (makeGraph r1) (makeGraph r2)
-}
