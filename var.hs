import Numeric

splits :: [a] -> [([a],[a])]
splits [] = [([],[])]
splits (x:xs) = [ (x:a,b) | (a,b) <- splits xs] ++ [(a,x:b) | (a,b) <- splits xs]

eqsplits :: [a] -> [([a],[a])]
eqsplits [] = [([],[])]
eqsplits xs = [ (a,b) | (a,b) <- splits xs, (sum a) == (sum b)]


setprecision :: Int -> Float -> Float
setprecision prec a = read $ showFFloat (Just prec) a ""

safeLog :: Float -> Maybe Float
safeLog x | x > 0 = Just (setprecision 3 (log x))
	  | otherwise = Nothing


data Arv a = Vazia | No a (Arv a) (Arv a) deriving (Eq, Show)

insertArvore :: Ord a => a -> Arv a -> Arv a
insertArvore x Vazia = No x Vazia Vazia
insertArvore x (No a esq dir) 	| x == a || x < a = No a (insertArvore x esq) dir
				| otherwise = No a esq (insertArvore x dir)
