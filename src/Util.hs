module Util where

import Data.Ord
import Data.List
import qualified Data.Set as S

rmdups :: (Ord a, Eq a) => [a] -> [a]
--rmdups = map head . group . sort

rmdups x = S.toList $ S.fromList x

sortAndGroupBy p = groupBy (equating p) . sortBy (comparing p)
  where equating p_ x y = (p_ x) == (p_ y)

regroup :: [a] -> [(a, a)]
regroup [] = []
regroup [_] = []
regroup (x:xs) = zip (repeat x) xs
