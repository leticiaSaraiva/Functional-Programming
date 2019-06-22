data Arv a = Vazia | No a (Arv a) (Arv a) deriving (Show, Eq)

nivel :: Int -> Arv a -> [a]
nivel _ Vazia = []
nivel 0 (No x esq dir) = [x]
nivel n (No x esq dir) = nivel (n-1) esq ++ nivel (n-1)  dir


inserir :: Ord a => a -> Arv a -> Arv a
inserir x Vazia = (No x (Vazia) (Vazia))
inserir x (No y esq dir)
        | x<y = inserir x esq
        | x>y = inserir x dir
        | otherwise = (No y esq dir)


maisesq :: Arv a -> a
maisesq (No x Vazia _) = x
maisesq (No _ esq _) = maisesq esq

maisdir (No x _ Vazia) = x
maisdir (No _ _ dir) = maisdir dir

removerEsq Vazia = Vazia
removerEsq (No x Vazia Vazia) = Vazia
removerEsq (No x esq _) = removerEsq esq


remover x Vazia = Vazia -- não ocorre
remover x (No y Vazia Vazia) = if x == y then Vazia else (No y Vazia Vazia)
remover x (No y Vazia dir ) = if  x == y then  (No (maisesq dir) Vazia dir)
                                                    else if x < y then (No y Vazia dir)
                                                    else remover x dir  -- um descendente
remover x (No y esq Vazia ) =  if x == y then (No (maisdir esq) esq Vazia)
                                                    else if x < y then (No y esq Vazia)
                                                    else remover x Vazia -- um descendente
remover x (No y esq dir ) -- dois descendentes
        | x<y = remover x esq
        | x>y = remover x dir
        | x==y = (No (maisesq dir) esq dir)


listar :: Ord a => Arv a -> [a]
listar Vazia = []
listar (No x esq dir) = listar dir ++ [x] ++ listar esq
