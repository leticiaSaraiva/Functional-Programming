fact 0 = 1
fact n = n * fact (n-1)

aproximae n = foldl (\z x-> (1.0/fromIntegral(fact x)) + z) 0 [0..n]