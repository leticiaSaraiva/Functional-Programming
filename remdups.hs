
remdups xs = foldr (\x z-> if elem x z then z else x : z) [] xs