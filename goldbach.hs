goldbach :: Int -> [(Int,Int,Int)]

crivo [] = []
crivo (x:xs) = x : crivo (filter (\y -> (mod y x) /= 0) xs)

primos n = crivo [2..n]

goldbach n = [ head [(x,y,z) |  y<-primos n, z<-primos n, y+z == x] | x<-[4,6..n]]
