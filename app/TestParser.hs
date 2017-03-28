import Lib

import System.Environment
import Data.Either



main :: IO ()
main = do --f <- getContents
          fs@[_, _] <- getArgs
          ps <- mapM parseVerilog fs
          case rights ps of
            [r1, r2]  -> do --print "ok"
                            putStrLn $ showGraph g1
                            putStrLn $ showGraph g2
                            putStrLn $ showGraph $ gu
                            --hPutStrLn stderr $ "// 10 " ++ show (createBDD gu 10)
                            --hPutStrLn stderr $ "// 3  " ++ show (createBDD gu 3)
                            --hPutStrLn stderr $ show (calculateAllBDDs gu)
                            where
                              g1 = makeGraphV [r1]
                              g2 = makeGraphV [r2]
                              gu = makeGraphV [r1, r2]
            x -> error $ show (x,lefts ps)
