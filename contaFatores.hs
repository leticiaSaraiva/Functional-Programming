contaFatores 0 p = 0 
contaFatores n p | mod n p == 0 = contaFatores (div n p) p +1
		         | otherwise = 0