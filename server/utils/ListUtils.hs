module ListUtils
  ( indexOf
  ) where

import           Data.List                      ( foldl' )

indexOf :: (Num p, Eq a) => a -> [a] -> p
indexOf _ [] = -1
indexOf item [x] | item == x = 0
                 | otherwise = -1
indexOf item xs = fst $ until foundItem increment (0, xs)
 where
  foundItem (_, xs') = null xs' || (head xs' == item)
  increment (acc, xs') = (acc + 1, drop 1 xs')
