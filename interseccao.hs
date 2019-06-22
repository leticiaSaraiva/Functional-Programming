interseccao :: [Int] -> [Int] -> [Int]
interseccao xs ys = [n | n <- xs, x <- ys, n == x]