frequencia 0 p = 0
frequencia n p 	| mod n p == 0 = frequencia (div n p) p + 1
	        	| otherwise = 0	
crivo [] = []
crivo (x:xs) = x : crivo (filter (\y -> (mod y x) /= 0) xs)

primos n = crivo [2..n]
		
primeFactors n = [(x,frequencia n x) | x<-primos n, mod n x == 0]