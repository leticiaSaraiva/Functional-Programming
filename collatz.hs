collatz 1 = [1]
collatz n   | mod n 2 == 0 = n : collatz (div n 2)
            | otherwise = n : collatz (3*n+1)