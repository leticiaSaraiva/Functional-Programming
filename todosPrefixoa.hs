todosPrefixos :: [Int] -> [[Int]]
todosPrefixos xs = [take n xs | n <- [0..(length xs)]]