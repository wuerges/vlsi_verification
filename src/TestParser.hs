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


showGraph g = showDot $ fglToDot $ gmap (\(is, n, _, os) -> (is, n, show n, os)) g

main :: IO ()
main = do --f <- getContents
          [f, i] <- getArgs
          p <- parseVerilog f
          case p of
            Right r -> do --print "ok"
                      --print r
                          putStrLn $ showGraph g
                          hPutStrLn stderr $ "// 0 " ++ show (createBDD g 0)
                          hPutStrLn stderr $ "// 1 " ++ show (createBDD g 1)
                          hPutStrLn stderr $ "// 2 " ++ show (createBDD g 2)
                          hPutStrLn stderr $ "// 3 " ++ show (createBDD g 3)
                          hPutStrLn stderr $ "// 4 " ++ show (createBDD g 4)
                          hPutStrLn stderr $ "// 5 " ++ show (createBDD g 5)
                          hPutStrLn stderr $ "// 6 " ++ show (createBDD g 6)
                          hPutStrLn stderr $ "// 7 " ++ show (createBDD g 7)
                            where vi = verilogToInt r (attIndexV emptyIndex r)
                                  g = makeGraphV vi
                                  bdd = createBDD g (read i)
            Left l ->  error $ show l
