module TestParser where

import VerParser
import Verilog
import Graph
import Algo
import BDD

import System.Environment
import Data.Graph.Inductive
import System.IO
import Data.Graph.Inductive.Dot
import Control.Monad
import Data.Either



showGraph g = showDot $ fglToDot $ gmap (\(is, n, _, os) -> (is, n, show n, os)) g

main :: IO ()
main = do --f <- getContents
          fs@[f1, f2] <- getArgs
          ps <- mapM parseVerilog fs
          case rights ps of
            [r1, r2]  -> do --print "ok"
                            --putStrLn $ showGraph g1
                            --putStrLn $ showGraph g2
                            putStrLn $ showGraph $ g1 `union` g2
                      --print r
                          -- putStrLn $ showGraph g
                          {-
                          hPutStrLn stderr $ "// 0 " ++ show (createBDD g 0)
                          hPutStrLn stderr $ "// 1 " ++ show (createBDD g 1)
                          hPutStrLn stderr $ "// 2 " ++ show (createBDD g 2)
                          hPutStrLn stderr $ "// 3 " ++ show (createBDD g 3)
                          hPutStrLn stderr $ "// 4 " ++ show (createBDD g 4)
                          hPutStrLn stderr $ "// 5 " ++ show (createBDD g 5)
                          hPutStrLn stderr $ "// 6 " ++ show (createBDD g 6)
                          hPutStrLn stderr $ "// 7 " ++ show (createBDD g 7)
                          -}
                            where i = foldl attIndexV emptyIndex [r1, r2] 
                                  v1 = verilogToInt r1 i
                                  v2 = verilogToInt r2 i
                                  g1 = makeGraphV v1
                                  g2 = makeGraphV v2
                                  bdd = bddReduce $ createBDD g1 0
            [] -> error $ show (lefts ps)                                  
