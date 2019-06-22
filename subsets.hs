data Mobile = Pendente Int | Barra Mobile Mobile deriving (Eq, Show)

subsets []     = [[]]
subsets (x:xs) = subsets xs ++ [ ys++[x] | ys <- subsets xs]

eqsplits :: [a] -> [([a],[a])]
eqsplits [] = [([],[])]
eqsplits (x:xs) | sum([(x:a,b) | (a,b) <- eqsplits xs]) == sum([ (a,x:b) | (a,b) <- eqsplits xs]) = [(x:a,b) | (a,b) <- eqsplits xs] ++ [ (a,x:b) | (a,b) <- eqsplits xs]
		| otherwise = eqsplits xs



makeBMobile :: [Int] -> Maybe Mobile
makeBMobile [x] = Just (Pendente x)
makeBMobile xs  | not (null ys) = Just (Barra a b)
                | otherwise = Nothing
	where 
	ys = eqsplits xs
        (a,b) = head ys	


