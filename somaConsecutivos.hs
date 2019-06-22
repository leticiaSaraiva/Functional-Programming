pares :: [Int] -> [(Int, Int)]
pares xs = zip xs (tail xs)

somaConsecutivos :: [Int] -> [Int]
somaConsecutivos xs = [x+y | (x,y) <- pares xs]