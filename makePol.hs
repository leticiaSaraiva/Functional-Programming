import Polinomio

makePol []     = polZero
makePol (x:xs) =
 | x == 0 = makePol xs
 | otherwise = consPol x n (makePol xs) 
 where
 n = length xs  

--aux xs = zip xs (makAux xs)  

--makAux [] = []
--makAux xs = [length xs] ++ makAux (tail xs)

{-
derivada p 	| n == 0 = polZero
		| otherwise = consPol (n*b) (n-1) (derivada r)
	where 
	   n = grau p 
	   b = coefLider p 
	   r = restoPol p 
-}


