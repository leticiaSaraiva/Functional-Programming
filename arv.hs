data ArvoreExpressao = No (Int->Int->Int) ArvoreExpressao ArvoreExpressao| Folha Int 

data Expr = Val Int
 | Soma Expr Expr
 | Mult Expr Expr
 | Div Expr Expr
 | Sub Expr Expr
 | Mod Expr Expr
 deriving (Read,Eq,Show)

fromExpr :: Expr -> ArvoreExpressao
fromExpr (Val a) = Folha a
fromExpr (Soma a b) = No (+) (fromExpr a) (fromExpr b)
fromExpr (Mult a b) = No (*) (fromExpr a) (fromExpr b)
fromExpr (Div a b) = No (div) (fromExpr a) (fromExpr b)
fromExpr (Sub a b) = No (-) (fromExpr a) (fromExpr b)
fromExpr (Mod a b) = No (mod) (fromExpr a) (fromExpr b)

eval :: ArvoreExpressao -> Int
eval (Folha a) = a
eval (No (+) a b) = (eval a) + (eval b)
eval (No (*) a b) = (eval a) * (eval b)
eval (No (div) a b) = div (eval a) (eval b)
eval (No (-) a b) = (eval b) - (eval a)
eval (No (mod) a b) = mod (eval a) (eval b)

showExpr :: Expr -> String
showExpr (Val a) = show a
showExpr (Soma a b) = "(" ++ (showExpr a) ++ "+" ++ (showExpr b) ++ ")"
showExpr (Mult a b) = "(" ++ (showExpr a) ++ "*" ++ (showExpr b) ++ ")"
showExpr (Div a b) = "(" ++ (showExpr a) ++ "/" ++ (showExpr b) ++ ")"
showExpr (Sub a b) = "(" ++ (showExpr a) ++ "-" ++ (showExpr b) ++ ")"
showExpr (Mod a b) = "(" ++ (showExpr a) ++ "%" ++ (showExpr b) ++ ")"
