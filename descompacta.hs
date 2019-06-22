descompacta :: [(a, b)] -> ([a], [b]) 

descompacta xs = ([a | (a,b) <- xs], [b |  (a,b) <- xs])
