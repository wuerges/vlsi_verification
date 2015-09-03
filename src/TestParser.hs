module TestParser where

import VerParser
import Verilog
import Graph
import Algo
import BDD

import System.Environment
import Data.Graph.Inductive
import Data.Graph.Inductive.Dot


showGraph g = showDot $ fglToDot $ gmap (\(is, n, _, os) -> (is, n, show n, os)) g


main :: IO ()
main = do --f <- getContents
          [f] <- getArgs
          p <- parseVerilog f
          case p of
            Right r -> do --print "ok"
                      --print r
                          putStr $ showGraph $ makeGraphV vi
                            where vi = verilogToInt r (attIndexV emptyIndex r)
            Left l ->  error $ show l
