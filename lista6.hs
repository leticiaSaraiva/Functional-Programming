--Questão 1
{-safeLog x | x > 0 = Just (log x)
	  | otherwise = Nothing 

--Questão 2
--type Ponto = (Float, Float)

dist :: Ponto -> Ponto -> Float
dist (x1,x2) (y1,y2) = sqrt(((x1-x2)*(x1-x2)) + ((y1-y2)*(y1-y2)))

comprimento :: [Ponto] -> Float
comprimento [] = 0
comprimento xs = sum [dist a b | (a,b) <- l]
	where l = zip xs (tail xs)

--Questão 3
data Ponto = Pt Float Float

type Regiao = Ponto -> Bool

retang :: Ponto -> Ponto -> Regiao
retang (x1,x2) (y1, y2) = (\(x,y) -> (x >= x1) && (x <= x2) && (y >= y1) && (y <= y2))

--circ :: Ponto -> Raio -> Regiao
circ p r = (\x -> (dist p x) <= r)
-}
--Questão 6
data Arv a = Vazia | No a (Arv a) (Arv a) deriving (Eq , Show)

arv1 = Vazia
arv2 = No 2 arv1 arv1
arv3 = No 4 arv2 arv1
arv4 = No 5 arv3 arv2

tamanhoArv Vazia = 0
tamanhoArv (No x esq dir) = 1 + (tamanhoArv esq) + (tamanhoArv dir)

altura Vazia = 0
altura (No x esq dir) = 1 + max (altura esq) (altura dir)

sumArv Vazia = 0
sumArv (No x esq dir) = x + (sumArv esq) + (sumArv dir)

nivel _ Vazia = []
nivel 0 (No x esq dir) = [x]
nivel n (No x esq dir) = []




