data LinkedList a = Vazia | No a (LinkedList a) deriving (Eq, Show)

fromList :: [a] -> LinkedList a
fromList [] = Vazia
fromList (x:xs) = No x (fromList xs)


toList :: LinkedList a -> [a]
toList Vazia = []
toList (No x xs) = [x] ++ toList xs 


append :: a -> LinkedList a -> LinkedList a
append a Vazia = No a Vazia
append a (No x xs) = No x (append a xs)


reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList Vazia = Vazia
reverseLinkedList linked = fromList (reverse (toList linked)) 
