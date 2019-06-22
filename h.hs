metade xs = (take tam xs, drop tam xs)
	where tam = div (length xs) 2

----------------------------------------------------------

maior xs = maximum xs
posi x [] = 0
posi x (y:xs) | x == y = 1
	      | otherwise = posi x xs + 1
maiorPosi xs = (a, posi a xs)
	where a = maior xs

----------------------------------------------------------

removPosi 1 (x:xs) = xs
removPosi n (x:xs) = x : removPosi (n-1) xs

----------------------------------------------------------
removeRep [] = []
removeRep (x:xs) | elem x xs = removeRep xs
		 | otherwise = x : removeRep xs
uniao1 xs ys = removeRep (xs ++ ys) 

uniao [] xs = xs
uniao xs [] = xs
uniao (x:xs) (y:ys) | x < y = x : uniao xs (y:ys)
		    | x == y = x : uniao xs ys
		    | otherwise = y : uniao (x:xs) ys

intersecao [] xs = []
intersecao xs [] = []
intersecao (x:xs) (y:ys) | x < y = intersecao xs (y:ys)
			 | x == y = x : intersecao xs ys
			 | otherwise = intersecao (x:xs) ys 







