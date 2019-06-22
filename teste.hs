
aux xs = zip xs (makAux xs)  


makAux [] = []
makAux xs = [length xs] ++ makAux (tail xs)
