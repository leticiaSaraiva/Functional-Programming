data MultiSet a = MultiSet [(a,Int)] deriving (Show)

insere n (MultiSet xs) = MultiSet (insereAux n xs)

insereAux n [] = [(n,1)]
insereAux n ((a,b):xs)	| n == a = ((a,b+1):xs)
			| n < a  = ((n,1):(a,b):xs)
			| n > a = ((a,b):(insereAux n xs))
			
