vogais = "A, E, I, O, U, a, e, i, o, u"
duplicarFold xs = foldr(\x z -> if elem x vogais then x:x:z else x:z) [] xs
