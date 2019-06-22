--1) Menor de dois
menorDois x y | x < y = x
	      | otherwise = y

--2) Menor de três
menorTres x y z = menorDois (menorDois x y) z

--3) Fatorial
fatorial 0 = 0
fatorial n = product [1..n]

--4) Fibonacci
fibonacci 1 = 0
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

--5) Elemento
elemento n xs = head (drop n xs)

--6) Pertence
pertence n xs = elem n xs

--7) Total
total [] = 0
total [x] = 1
total (x:xs) = 1 + total xs

--8) Maior
--maior (x:xs) = 

--9) Frequência
frequencia n [] = 0
frequencia n (x:xs) | x == n = f + 1
		    | otherwise = f
	where f = frequencia n xs

--10) Unico
--unico n [] = False
--unico n [n] = True
--unico n (x:xs)  | x == n && length (unico n xs) == 0 = True
--		| otherwise = False 

--11) Maiores Que
maioresQue n [] = []
maioresQue n (x:xs) | x > n = x:maioresQue n xs
		    | otherwise = maioresQue n xs

--12) Concat
concat1 xs ys = xs ++ ys

--13) Cauda
calda xs = drop 1 xs --tail xs

--14) Corpo
corpo xs =  take (length xs -1) xs --init xs

--15) Unique


--16) Menores
menores n [] = []
menores n (x:xs) | x < n = x:menores n xs
		 | otherwise = menores n xs

--18) Reverso
reverso xs = foldr (\x acc -> acc ++ [x]) [] xs

--19) Divide
divide [] n = ([],[])
divide xs n = (take n xs, drop n xs)

--20) Intercala
intercal [] [] = []
intercal xs [] = xs
intercal [] xs = xs
intercal (x:xs) (y:ys) = x:y:intercal xs ys














