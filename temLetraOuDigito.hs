import Data.Char

temLetraOuDigito :: String -> Bool

temLetraOuDigito a = foldr(\x acc -> isLetter x || isDigit x || acc) False a
