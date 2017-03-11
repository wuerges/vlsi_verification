import Lib
import System.Environment
import Data.Either
import Text.Printf

main :: IO ()
main = do
          args <- getArgs
          mapM_ doEquiv args

doEquiv f1 =  do
          putStr $ "Testing <" ++ f1 ++ "> ... "
          p1 <- parseVerilog f1
          case p1 of
            Right r1 -> outputResult $ equivKuelmann97_2 r1 r1
            Left e -> error $ show e


outputResult (e, rs) = do
  mapM_ outputResult' $ zip [1..] rs
  case e of
    Left m -> error $ "could not check if equivalent or not"
    Right r -> if r then putStrLn $ "OK."
                    else error $ "Not OK."

outputResult' :: (Int, String) -> IO()
outputResult' (n, comment) = do
  writeFile (printf "debug_graph_%05d.dot" n)
    comment

