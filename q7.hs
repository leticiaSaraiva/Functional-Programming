data MultiSet a = MultiSet [(a,Int)] deriving (Show)

conta f (MultiSet xs) = contaAux f xs

contaAux f [] = 0
contaAux f ((a,b):xs) | f a = b + (contaAux f xs)
		      | otherwise =  contaAux f xs
