import TestBase

--import Test.QuickCheck
import BDDGraph
import Test.HUnit
import Data.Graph.Inductive
import Text.Printf

initials_3_4_5 = runBDDState [3,4,5] $ return ()
initials_3_4_5_dups = runBDDState [3,4,5] $ do
  dupNode (Just 10) 3
  dupNode (Just 11) 4
  dupNode (Just 12) 5

initial_3 = runBDDState [2] $
  bddAndMany (Just 10) [B 2]

bddSpace = runBDDState [3,4,5] $ do
  b1 <- bddAndRepr 10 (B 3) (B 4)
  b2 <- bddAndRepr 20 (B 4) (B 5)
  --bddAndRepr 8 b1 (B 5)
  bddAndMany (Just 30) [b2, B 3]
  bddAndMany (Just 40) [b1, B 5]
  reduceAll

simpleDup =  runBDDState [3] $ do
  (l, r) <- getSons 3
  newParent (Just 10) 3 (l, r)
  reduceAll

equate_4_5 = runBDDState [3] $ do
  equate 4 5



--getGM = fst . snd . fst
--getEq = snd . snd . fst
--showG = showBDD . getGM

getGM = fst . snd
getEq = snd . snd
--showG = showBDD . getGM

writeLogs m = mapM_ writeLog $ zip [1..] (snd . fst $ m)
  where
    writeLog :: (Int, String) -> IO ()
    writeLog (n, txt) = writeFile (printf "debug_log_%03d.dot" n) txt

t1 = TestCase $ do
  --putStrLn $ "\n=> SimpleDup: \n\n" ++ showG simpleDup ++ "\n" ++ show (getEq simpleDup)  ++"\n\n"
  --putStrLn $ "\n=> bddSpaceDup: \n\n" ++ showG bddSpace ++ "\n" ++ show (getEq bddSpace) ++ "\n\n"
  --writeLogs bddSpace
  assertEqual "There must be only one equate" 1 (length $ getEq bddSpace)
  assertBool "The equate must be (30,40)" ((elem (30, 40) $ getEq bddSpace) || (elem (40, 30) $ getEq bddSpace))

t2 = TestCase $
  assertEqual "equate_4_5" (4,5) (head . getEq $ equate_4_5)

t3 = TestCase $ do
  let g = getGM $ equate_4_5
      ls = layers g
  assertEqual "layers_4_5" [[0,1],[3]] ls

t4 = TestCase $ do
  let g = getGM $ initials_3_4_5
      ls = layers g
  assertEqual "initials_3_4_5" [[0,1],[3], [4], [5]] ls

t5 = TestCase $ do
  let g = getGM $ initials_3_4_5_dups
      ls = layers g
  --putStrLn $ "\nG: -> " ++ show g
  assertEqual "initials_3_4_5" [[0,1],[3, 10], [4, 11], [5, 12]] ls

t6 = TestCase $ do
  let g = getGM $ bddSpace
      ls = layers g
  assertEqual "layers_3_4_5" 4 (length ls)

t7 =
  let g = getGM $ initial_3
      ls = layers g
   in TestCase $ do
     --putStrLn $ "\nG: -> " ++ showBDD g
     assertEqual "layers_3" 2 (length ls)


main = do
  r <- runTestTT $ TestList [t1, t2, t3, t4, t5, t6, t7]
  print r
