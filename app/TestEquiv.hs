import Lib
import System.Environment
import Data.Either
import Text.Printf

main :: IO ()
main = do
          [f1, f2] <- getArgs
          doMain f1 f2

doMain f1 f2 =  do
          p1 <- parseVerilog f1
          p2 <- parseVerilog f2
          case rights [p1, p2] of
            --[r1, r2] -> outputResult $ retryEquivLimited_2 r1 r2
            [r1, r2] -> outputResult $ equivKuelmann97_2 r1 r2
            _ -> error $ show $ lefts [p1, p2]


outputResult (e, rs) = do
  mapM_ outputResult' $ zip [1..] rs
  case e of
    Left m -> error $ "could not check if equivalent or not"
    Right r -> putStrLn $ "Result: " ++ show r

outputResult' :: (Int, String) -> IO()
outputResult' (n, comment) = do
  writeFile (printf "debug_graph_%05d.dot" n)
    comment

