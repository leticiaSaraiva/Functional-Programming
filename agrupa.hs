vazia xss = elem [] xss
agrupa xss  | elem [] xss = [] 
            | otherwise = (map head xss) : agrupa(map tail xss)