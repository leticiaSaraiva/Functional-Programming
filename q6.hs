data MultiSet a = MultiSet [(a,Int)] deriving (Show)

deleteAux x n [] = []
deleteAux x n ((a,b):xs) | x == a && n < b = ((a,(b-n)):xs)
			 | x == a && (n > b || n == b) = xs
			 | otherwise = (a,b):(deleteAux x n xs)

delete a n (MultiSet xs) = MultiSet (deleteAux a n xs)

