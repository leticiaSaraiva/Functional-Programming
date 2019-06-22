descompacta [] = ([],[])
descompacta xs = ([a | (a,b)<-xs],[b | (a,b)<-xs])

type Item = String
total xs ys = sum [b | (a,b)<-xs, y<-ys, a==y]

preco = [("Leite", 2.0), ("Manteiga", 2.5), ("Batata", 4.0), ("Brocolis", 2.0), ("Cenoura", 2.2)]


data Prop = And Prop Prop | Or Prop Prop | Not Prop | Val Bool deriving (Show, Eq)

prop1 = (And (Val True) (Or (Val False) (Val True)))
prop2 = (Not (And (Val True) (Val False)))


eval (Val a) = a
eval (And a b) = (eval a) && (eval b)
eval (Or a b) = (eval a) || (eval b)
eval (Not a) = not (eval a)

isPrefix xs ys 	| take (length xs) ys == xs = True
		| otherwise = False
