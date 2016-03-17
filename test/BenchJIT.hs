import VerParser
import Algo
import Kuelmann97

import Data.List
import Test.HUnit
import System.Environment
import Equivalence
import Data.Either
import Control.Applicative
import System.Timeout
import LLVMJIT
import Graph
import Verilog
import TestBase
import Data.Time

makeTest :: String -> Test
makeTest n = TestCase $ do
    p <- parseVerilog n
    case p of
      Right r -> do
        let g = makeGraphV . runIndex $ verilogToInt r

        start <- getCurrentTime
        r' <- runJITG g (replicate (length (inputs g)) True) Nothing
        stop <- getCurrentTime
        putStrLn $  "\n====================\nFinished simulation in: " ++ show (diffUTCTime stop start)
        case r' of
          Right (_, x) -> putStrLn $ "\n==============> Success!"
          Left l  -> assertFailure $ show l
      Left l  -> assertFailure $ show l

tests =
  TestList $ map makeTest fileNames

main = runTestTT tests >>= print
