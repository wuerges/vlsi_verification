module TestParser where

import VerParser
import Verilog
import Graph
import Algo
import BDD

import System.Environment
import Data.Graph.Inductive
import System.IO
import Control.Monad
import Data.Either



main :: IO ()
main = do --f <- getContents
          fs@[f1, f2] <- getArgs
          ps <- mapM parseVerilog fs
          case rights ps of
            [r1, r2]  -> do --print "ok"
                            --putStrLn $ showGraph g1
                            --putStrLn $ showGraph g2
                            putStrLn $ showGraph $ g1 `union` g2
                            --hPutStrLn stderr $ "// 10 " ++ show (createBDD gu 10)
                            --hPutStrLn stderr $ "// 3  " ++ show (createBDD gu 3)
                            --hPutStrLn stderr $ show (calculateAllBDDs gu)
                            where i = foldl attIndexV emptyIndex [r1, r2]
                                  v1 = verilogToInt r1 i
                                  v2 = verilogToInt r2 i
                                  g1 = makeGraphV v1
                                  g2 = makeGraphV v2
                                  gu = g1 `union` g2
            [] -> error $ show (lefts ps)
