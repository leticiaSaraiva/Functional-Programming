import Stack

polonesa :: String -> Int
polonesa xs = polonesaAux (words xs) (emptyStack)

polonesaAux [] p = top p
polonesaAux (x:xs) p 
 | x == "+"  = polonesaAux xs (push (val1 + val2) q2)
 | x == "-"  = polonesaAux xs (push (val2 - val1) q2)
 | x == "*"  = polonesaAux xs (push (val1 * val2) q2)
 | x == "/"  = polonesaAux xs (push (div val2 val1) q2)
 | otherwise = polonesaAux xs (push (converte x) p)
 where
 val1 = top p
 q1 = pop p
 val2 = top q1
 q2 = pop q1	

converte :: String -> Int
converte x = (read :: String -> Int) x 
