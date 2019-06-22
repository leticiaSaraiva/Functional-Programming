data ArvoreExpressao = No (Int->Int->Int) ArvoreExpressao ArvoreExpressao| Folha Int 

data Expr = Val Int | Soma Expr Expr | Mult Expr Expr | Div Expr Expr | Sub Expr Expr | Mod Expr Expr deriving (Read,Eq,Show)

fromExpr :: Expr -> ArvoreExpressao
fromExpr (Val i) = Folha i
fromExpr (Soma a b) = No (+) (fromExpr a) (fromExpr b)
fromExpr (Mult a b) = No (*) (fromExpr a) (fromExpr b)
fromExpr (Div a b) = No (div) (fromExpr a) (fromExpr b)
fromExpr (Sub a b) = No (-) (fromExpr a) (fromExpr b)
fromExpr (Mod a b) = No (mod) (fromExpr a) (fromExpr b)


eval :: ArvoreExpressao -> Int
eval Soma (Val a) (Val b) = a+b
eval No (*) (Val a) (Val b) = a*b
eval No (div) (Val a) (Val b) = div a b
eval No (-) (Val a) (Val b) = a-b
eval No (mod) (Val a) (Val b) = mod a b
