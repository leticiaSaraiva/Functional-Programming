fatoracao_aux 1 _ = []
fatoracao_aux n (x:xs) | mod n x == 0 = x : fatoracao_aux(div n x) (x:xs)
                       | otherwise = fatoracao_aux n xs

fatoracao n = fatoracao_aux n [2..n]