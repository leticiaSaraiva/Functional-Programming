import Data.List (insert)
 
insertSort :: Ord a => [a] -> [a]
insertSort = foldr insert []