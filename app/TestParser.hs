import Lib

import Graph
import System.Environment
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
                            putStrLn $ showGraph $ gu
                            --hPutStrLn stderr $ "// 10 " ++ show (createBDD gu 10)
                            --hPutStrLn stderr $ "// 3  " ++ show (createBDD gu 3)
                            --hPutStrLn stderr $ show (calculateAllBDDs gu)
                            where [v1, v2] = runIndex $ mapM verilogToInt [r1, r2]
                                  g1 = makeGraphV v1
                                  g2 = makeGraphV v2
                                  (gu, _, _) = g1 `union` g2
            [] -> error $ show (lefts ps)
