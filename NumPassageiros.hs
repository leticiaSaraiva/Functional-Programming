data Trem a = Vagao a (Trem a) | Vazio deriving Show

data Carga = SemCarga | Passageiro Quantidade | Mercadoria Peso deriving Show

type Quantidade = Int

type Peso = Int

numPassageiros :: Trem Carga -> Int
numPassageiros Vazio = 0
numPassageiros (Vagao (Passageiro n) restoTrem) = n + (numPassageiros restoTrem)
numPassageiros (Vagao (_) restoTrem) = numPassageiros restoTrem
