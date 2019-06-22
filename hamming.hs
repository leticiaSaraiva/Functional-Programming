mergeUnic xs [] = xs
mergeUnic [] ys = ys
mergeUnic (x:xs) (y:ys) | x < y = x : mergeUnic xs (y:ys)
                        | y < x = y : mergeUnic (x:xs) ys
                        | otherwise = x : mergeUnic xs ys
                        
hamming = 1 : mergeUnic (map (2*) hamming) (mergeUnic (map (3*) hamming) (map (5*) hamming))
