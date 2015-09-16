module GenTestHarness where

import Verilog
import VerParser

import System.Environment
import System.IO
import Data.List

mkSignals r = intercalate ", " (_inputs r ++ _outputs r)
mkOutputs r = intercalate ", " (_outputs r)
mkInputs r = intercalate ", " (_inputs r)
randomSignals r = " "

mkMonitor r = "\"" ++ intercalate ", " (map m1 $ _inputs r ++ _outputs r) ++ "\""
  where m1 s = s ++ "=%b"

harness r = "module harness (" ++ mkSignals r ++ "); \n\
            \output " ++ mkOutputs r ++ ";           \n\
            \input " ++ mkInputs r ++ ";             \n\
            \                                      \n\
            \top toptop1(" ++ mkSignals r ++ ");   \n\
            \                                      \n\
            \initial                               \n\
            \begin                                 \n\
            \  $monitor(" ++ mkMonitor r ++ ",       \n\
            \           " ++ mkSignals r ++ ");      \n\
            \                                      \n\
            \                                      \n\
            \ " ++ randomSignals r ++ "              \n\
            \end                                   \n\
            \                                      \n\
            \endmodule                             \n"


main :: IO ()
main = do
          [f] <- getArgs
          p <- parseVerilog f
          case p of
            Right r -> do putStr $ harness r
            Left l -> error $ show l
