data MConj a = Vazio | No a Int (MConj a) (MConj a) deriving (Show, Eq)

ocorre :: Ord a => a -> MConj a -> Int
ocorre n Vazio = 0
ocorre n (No a m esq dir) | n == a = m
			  | n < a = ocorre n esq
			  | otherwise = ocorre n dir 

--insere :: Ord a => a -> MConj a -> MConj a
insere n Vazio = No n 1 Vazio Vazio
insere n (No a m esq dir) | n == a = (No a (m+1) esq dir)
			  | n < a = (No a m (insere n esq) dir)
			  | otherwise = (No a m esq (insere n dir))	

toList :: MConj a -> [a]
toList Vazio = []
toList (No a m dir esq) = (replicate m a) ++ toList dir ++ toList esq


tamanho :: MConj a -> Int
tamanho Vazio = 0
tamanho (No a m esq dir) = m + (tamanho esq) + (tamanho dir)


fromList :: Ord a => [a] -> MConj a
fromList [] = Vazio
fromList xs = foldr insere Vazio (reverse xs)


--remove :: Ord a => a -> Int -> MConj a -> MConj a
remove x n Vazio = Vazio
remove x n (No a m esq dir) 	| x < a = No a m (remove x n esq) dir
				| x > a = No a m esq (remove x n dir)
				| n < m = No a (m-n) esq dir
				| esq == Vazio = dir
				| dir == Vazio = esq
				| otherwise = No z l esq (remove z l dir)
			where (z,l) = maisEsq dir

maisEsq (No x i Vazia _) = (x, i)
maisEsq (No x i esq dir) = maisEsq esq

