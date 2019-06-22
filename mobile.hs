data Mobile = Pendente Int | Barra Mobile Mobile deriving (Eq, Show)

makeMobile [x] = Pendente x
makeMobile xs = Barra (makeMobile m1) (makeMobile m2)
	where m1 = take (div (length xs) 2) xs
	      m2 = drop (div (length xs) 2) xs


splits [] = [([],[])]
splits (x:xs) = [ (x:a,b) | (a,b) <- splits xs] ++ [(a,x:b) | (a,b) <- splits xs]

eqsplits [] = [([],[])]
eqsplits xs = [ (a,b) | (a,b) <- splits xs, (sum a) == (sum b)]


makeBMobile [] = Nothing
makeBMobile [x] = Just (Pendente x)
makeBMobile xs 	| even (sum xs) = Just (Barra (makeMobile (fst q)) (makeMobile (snd q)))
		| otherwise = Nothing
	where q = head (eqsplits xs)

balanceado :: Mobile -> Bool
balanceado (Pendente a) = True
balanceado (Barra esq dir) 	| peso esq == peso dir = True
				| otherwise = False


peso :: Mobile -> Int
peso (Pendente a) = a
peso (Barra esq dir) = (peso esq) + (peso dir)
