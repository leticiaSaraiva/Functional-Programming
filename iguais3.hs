


iguais3 x y z | x==y && y==z = 3
              | x==y && y/=z = 2
              | x==z && y/=z = 2
              | y==z && y/=x = 2
              | otherwise = 0
