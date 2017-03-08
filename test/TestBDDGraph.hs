import TestBase

--import Test.QuickCheck
import BDDGraph
import Test.HUnit
import Data.Graph.Inductive
import Text.Printf

initials_3_4_5 = runBDDStateT [3,4,5] $ return ()
initials_3_4_5_dups = runBDDStateT [3,4,5] $ do
  dupNode (V 3 (Just 10))
  dupNode (V 4 (Just 11))
  dupNode (V 5 (Just 12))

bddSpace = runBDDStateT [3,4,5] $ do
  bddAndRepr 6 (B 3) (B 4)
  bddAndRepr 7 (B 4) (B 5)
  bddAndRepr 8 (B 7) (B 5)
  bddAndRepr 9 (B 3) (B 9)
  reduceAll

simpleDup =  runBDDStateT [3] $ do
  (l, r) <- getSons 3
  newParent (V 3 (Just 4)) (l, r)
  reduceAll

equate_4_5 = runBDDStateT [3] $ do
  equate (Just 4) (Just 5)



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
  assertBool "The equate must be either (8,9) or (9,8)" ((elem (8, 9) $ getEq bddSpace) || (elem (9, 8) $ getEq bddSpace))

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
  assertEqual "initials_3_4_5" [[0,1],[3, 6], [4, 7], [5, 8]] ls

t6 = TestCase $ do
  let g = getGM $ bddSpace
      ls = layers g
  assertEqual "layers_3_4_5" 4 (length ls)

main = do
  r <- runTestTT $ TestList [t1, t2, t3, t4, t5, t6]
  print r
