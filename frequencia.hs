frequencia :: Int -> [Int] -> Int
frequencia a [] = 0

frequencia a (x:xs) | x == a = 1 + frequencia a xs
                    | otherwise = frequencia a xs