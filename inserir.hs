inserir :: Int -> [Int] -> [Int]

inserir x [] = [x]
inserir x (y:ys)    | x <= y = x:(y:ys)
                    | otherwise = y:inserir x ys 

