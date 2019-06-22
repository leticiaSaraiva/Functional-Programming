import Queue

pula 0 q = q
pula n q = pula (n-1) novaF
	where 
	   novaF = enqueue (front q) (dequeue q)  

mostra k q | isEmpty q = []
	   | otherwise = morto : (mostra k resto)
	where 
	   pulado = pula (k-1) q
	   morto = front pulado
	   resto = dequeue pulado

josephus n k = mostra k (makeQueue [1..n]) 
