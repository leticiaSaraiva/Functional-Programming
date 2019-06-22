data ArvBin a = Vazia | No a ( ArvBin a ) ( ArvBin a ) deriving (Show)

zipTree :: ArvBin a -> ArvBin b -> ArvBin (a,b)
zipTree Vazia Vazia = Vazia
zipTree Vazia _ = Vazia
zipTree _ Vazia = Vazia
zipTree (No a esqA dirA) (No b esqB dirB) = No (a,b) (zipTree esqA esqB) (zipTree dirA dirB)

arv1 = No 6 (No 5 Vazia Vazia) (No 2 Vazia Vazia)
arv2 = No 5 (No 6 Vazia Vazia) (No 7 Vazia (No 2 Vazia Vazia))
