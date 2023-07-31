

data Lista a = Vazia | Cons a (Lista a) deriving (Show)



tamanho :: Lista a -> Int
tamanho Vazia = 0
tamanho (Cons x xs) = 1 + tamanho xs

--Árvore binária (quase) completa
data Tree a = Leaf a | Nodo (Tree a) a (Tree a) deriving (Show)

--let arvE = (Nodo (Leaf 2) 1 (Leaf 3))
--let arvD = (Nodo (Leaf 5) 4 (Leaf 6))
--let arv = arvE 0 arvD

buscar :: Eq a => a -> Tree a -> Bool
buscar x (Leaf y) = x == y
buscar x (Nodo l y r) = x == y || buscar x l || buscar x r 

achatar :: Tree a -> [a]
achatar (Leaf x) = [x]
achatar (Nodo l x r) = achatar l ++ [x] ++ achatar r 

achatarCentral :: Tree a -> [a]
achatarCentral (Leaf x) = [x]
achatarCentral (Nodo l x r) = [x] ++ achatarCentral l ++ achatarCentral r 

achatarPos :: Tree a -> [a]
achatarPos (Leaf x) = [x]
achatarPos (Nodo l x r) = achatarPos l ++ achatarPos r ++ [x] 

--
data ArvBin a = VaziaBin | Node (ArvBin a) a (ArvBin a ) deriving (Show)

--let arvE = (Node (Node VaziaBin 2 VaziaBin) 1 (Node VaziaBin 3 VaziaBin))

buscarBin :: Eq a => a -> ArvBin a -> Bool
buscarBin x VaziaBin = False
buscarBin x (Node l y r) = x == y || buscarBin x l || buscarBin x r 

data Arv a = ArvVazia | ArvNodo a [Arv a] deriving (Show)

buscarN :: Eq a => a -> Arv a -> Bool
buscarN x ArvVazia = False










