module Util where

import Data.List

rmdups :: (Ord a, Eq a) => [a] -> [a]
rmdups = map head . group . sort
