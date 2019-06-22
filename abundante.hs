abundante :: Int -> Bool
abundante n  | sum [x | x<-[1..n-1], n `mod` x == 0] > n = True
             | otherwise = False