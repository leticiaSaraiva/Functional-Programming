
removeLista xs ys = foldr (\x z-> if elem x xs then z else [x] ++ z) [] ys
