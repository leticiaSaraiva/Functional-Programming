divisores :: Int -> [Int]
divisores n = [x | x<-[1..n], mod n x == 0]