import LLVMJIT
import Data.List
import Test.HUnit



v1 = replicate 100 True
v2 = intersperse False v1
v3 = replicate 100 False
v4 = intersperse True v3
v5 = intersperse True v2
v6 = intersperse False v3



makeTest bs = TestCase (do putStrLn $ "\nTesting Foregin Marshalling"
                           p_bs <- allocToPtr bs
                           bs' <- freeToBool (length bs) p_bs
                           assertEqual "bs' == bs" bs bs')

tests = TestList $ map makeTest [v1, v2, v3, v4, v5, v6]


main = runTestTT tests >>= print

