data MultiSet a = MultiSet [(a,Int)] deriving (Show)
 
makeMultiSet xs = MultiSet (makeAux (selectSort xs))

makeAux [] = []
makeAux (x:xs) = (x, length (takeWhile (==x) (x:xs))) : (makeAux (dropWhile (==x) (x:xs)))

selectSort [] = []
selectSort xs = m : (selectSort (apaga (m) xs))
   where m = minimum xs
 
apaga y (x:xs)  | x == y = xs
	        | otherwise = x : (apaga y xs)

