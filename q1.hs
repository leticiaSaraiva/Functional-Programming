data Arvore a = Folha a | Ramo (Arvore a) (Arvore a) deriving (Show)

foldTree :: (a->b) -> (b->b->b) -> Arvore a -> b
foldTree f g (Folha a) = f a
foldTree f g (Ramo esq dir) = g (foldTree f g esq) (foldTree f g dir)

arv1 = Ramo (Folha 1) (Folha 2)
arv2 = Ramo (Folha 5) (Ramo (Folha 4) (Folha 3))
arv3 = Ramo (Ramo (Folha 5) (Folha 4)) (Ramo (Folha 2) (Ramo (Folha 1) (Folha 6)))


