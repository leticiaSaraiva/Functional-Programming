
inverteFold xs = foldr (\x z -> z ++ [x]) [] xs