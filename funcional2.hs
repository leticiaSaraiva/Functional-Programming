data ArvoreExpressao = No (Int->Int->Int) ArvoreExpressao ArvoreExpressao| Folha Int 

data Expr = Val Int | Soma Expr Expr | Mult Expr Expr | Div Expr Expr | Sub Expr Expr | Mod Expr Expr deriving (Read,Eq,Show)

fromExpr :: Expr -> ArvoreExpressao
fromExpr (Val p) = Folha p
fromExpr (Soma p1 p2) = No (fromExpr p1) fromExpr (Val Soma) fromExpr (p2)
fromExpr (Mult p1 p2) = No (fromExpr p1) fromExpr (Val Mult) fromExpr (p2)
fromExpr (Div p1 p2) = No (fromExpr p1) fromExpr (Val Div) fromExpr (p2)
fromExpr (Sub p1 p2) = No (fromExpr p1) fromExpr (Val Sub) fromExpr (p2)
--fromExpr (Mod p1 p2) = No (fromExpr p1) fromExpr (Val Mod) fromExpr (p2)
