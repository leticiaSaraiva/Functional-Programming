
calcPi n = foldl (\z x -> z + ((4*((-1)^(fromIntegral x)))/(2*(fromIntegral x)+1))) 0 [0..n]
