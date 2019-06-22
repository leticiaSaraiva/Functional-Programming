
paridadeFold xs | mod (foldr(\x z -> if x == True then z+1 else z) 0 xs) 2 == 0 = True
                | otherwise = False