
splitints [] = ([],[])
splitints (x:xs)    | mod x 2 == 0 = (i, x:p)
                    | otherwise = (x:i, p)  
                
                where (i,p) = splitints xs
                    
                 