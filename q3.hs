data Estrada = Cidade String | Bifurcacao Estrada Estrada

alcanca :: String -> Estrada -> Bool
alcanca c (Cidade a) = c == a
alcanca c (Bifurcacao esq dir) = (alcanca c esq) || (alcanca c dir)

estrada = Bifurcacao (Cidade "Quixada") (Bifurcacao (Bifurcacao (Cidade "Quixeramobim") (Cidade "Senador Pompeu")) (Cidade "Madalena "))
