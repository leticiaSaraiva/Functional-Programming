subconjunto :: [Int] -> [Int] -> Bool

subconjunto [] ys = True
subconjunto (x:xs) ys   | elem x ys = True && subconjunto xs ys
                        | otherwise = False