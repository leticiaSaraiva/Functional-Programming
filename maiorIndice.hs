maiorIndice [x] = (x, 0) 
maiorIndice (x:xs)  | x >= m = (x,0)
		            | otherwise = (m, i+1)
	where (m,i) = maiorIndice xs