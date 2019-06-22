import Data.Char
capitalises :: [Char] -> [Char]

capitalises n = [toUpper x | x <- n]