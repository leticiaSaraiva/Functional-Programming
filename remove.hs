remove :: Int -> [Int] -> [Int]

remove a [] = []

remove a (x:xs) | x==a = xs 
                | otherwise = x : remove a xs
