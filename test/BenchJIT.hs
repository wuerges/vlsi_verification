import VerParser
import Algo
import Kuelmann97

import Data.List
import Test.HUnit
import System.Environment
import Equivalence
import Data.Either
import Control.Applicative
import Control.Monad
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
            inputs_t = replicate (length (inputs g)) True

        start <- getCurrentTime
        optmodT <- compileF g
        stop <- getCurrentTime
        case optmodT of
          Right optmodule -> do
            startS <- getCurrentTime
            r' <- replicateM 100 $ runF g optmodule inputs_t
            stopS <- getCurrentTime
            putStrLn $  "\nFile:" ++ show n ++ "    compilation: "
                     ++ show (diffUTCTime stop start) ++ "     simulation: "
                     ++ show (diffUTCTime stopS startS)
          Left l  -> assertFailure $ show l
      Left l  -> assertFailure $ show l

tests =
  TestList $ map makeTest fileNames

main = runTestTT tests >>= print
