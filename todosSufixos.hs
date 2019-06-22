todosSufixos :: [Int] -> [[Int]]
todosSufixos xs = [reverse(take n (reverse xs)) | n <- [0..(length xs)]]