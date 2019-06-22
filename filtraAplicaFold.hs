
--filtraAplicaFold f p [] = []
--filtraAplicaFold f p (x:xs) | p x = f x : filtraAplicaFold f p xs
--                            | otherwise = filtraAplicaFold f p xs
                            
filtraAplicaFold f p xs = foldr(\x z -> if p x then f x : z else z) [] xs