neglist :: [Int] -> Int
neglist xs = length (filter (<0) xs)
