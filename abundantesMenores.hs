obter_div :: Int -> [Int]
-- recebe o número e retorna uma lista dos divisores desse número
obter_div num = [ x | x <- [1 .. num-1], ((mod num x) == 0)]
             
-- recebe o numero, retorna True se for abundante e False caso contrário
abundante :: Int -> Bool
abundante num
		| ((sum (obter_div num)) > num) = True
		| otherwise = False

-- retorna uma lista com todos os números abundantes até "n"
abundantesMenores :: Int -> [Int]
abundantesMenores n = [x | x <- [1 .. n], ((abundante x) == True)]