module Util where

--import Data.List
import qualified Data.Set as S

rmdups :: (Ord a, Eq a) => [a] -> [a]
--rmdups = map head . group . sort

rmdups x = S.toList $ S.fromList x
