module Main where

import Verilog
import VerParser

import System.Environment
import System.IO
import Data.List

mkSignals r = intercalate ", " (_inputs r ++ _outputs r)
mkOutputs r = intercalate ", " (_outputs r)
mkInputs r = intercalate ", " (_inputs r)
randomSignals r = " "


fib = (map fib' [0..] !!)
  where fib' 0 = 1
        fib' 1 = 1
        fib' n = fib (n-2) + fib (n-1)

mkMonitor r = "\"" ++ intercalate ", " (map m1 $ _outputs r) ++ "\""
  where m1 s = s ++ "=%b"


mkDisplay r = "$display(" ++ mkMonitor r ++ ", " ++ mkOutputs r ++ ");"

mkStep r ns = "#10 " ++ intercalate " " (map st $ zip (_inputs r) ns) ++ "\n" ++ mkDisplay r
  where st (i, n)  = i ++  " = " ++ show (number n)  ++ "; \n"
        number n = if n `mod` 3 == 0 then 1 else if n `mod` 7 == 0 then 1 else if n `mod` 13 == 0 then 1 else 0


mkManySteps _ [] = ""
mkManySteps r l  = mkStep r x ++ mkManySteps r xs
  where (x, xs) = splitAt (length $ _inputs r) l

fibs n = map fib [0..n]


harness r mn n = "module harness (" ++ mkOutputs r ++ "); \n\
                 \output " ++ mkOutputs r ++ ";           \n\
                 \reg " ++ mkInputs r ++ ";             \n\
                 \                                      \n\
                 \\n" ++ mn ++  " toptop1(" ++ mkSignals r ++ ");   \n\
                 \                                      \n\
                 \initial                               \n\
                 \begin                                 \n\
                 \                                      \n\
                 \ " ++ mkManySteps r (fibs n) ++ "         \n\
                 \                                      \n\
                 \ " ++ randomSignals r ++ "              \n\
                 \end                                   \n\
                 \                                      \n\
                 \endmodule                             \n"


main :: IO ()
main = do
          [f, mn, n] <- getArgs
          p <- parseVerilog f
          case p of
            Right r -> do putStr $ harness r mn (read n)
            Left l -> error $ show l
