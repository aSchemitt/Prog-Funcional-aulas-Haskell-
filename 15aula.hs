module ExRevisão where
--revisão e exercicios para a prova
--exercicios de revisão 2
--q1

digitos :: Int -> [Int]
digitos 0 = []
digitos n = digitos (n `div` 10) ++ [n `mod` 10]


armstrong :: Int -> Bool
--"normalzão"
--armstrong n = sum [ d^(length (digitos n)) | d <- (digitos n)]

--list comprehension
{- armstrong n = sum [d^t | d <- ds] == n
    where ds = digitos n 
          t = length (digitos n) -}
--map
armstrong n = sum (map (^t) ds) == n
        where ds = digitos n 
              t = length (digitos n)

lerInt :: IO Int
lerInt = readLn

main = do
    putStrLn "Digite um número:"
    numero <- lerInt
    putStrLn ("É armstrong: " ++ show (armstrong numero))


---------------------------------------------------------------------------------------------------------------------------------
--q2

data Tree =Leaf Int |Node Tree Tree deriving (Show)
--a)
leaves :: Tree -> [Int]
leaves (Leaf n)   = [n]
leaves (Node e d) = leaves e ++ leaves d

--b)
size :: Tree -> Int
--size a = length (leaves a)
size (Leaf _)   = 1
size (Node e d) = size e + size d

--c)
balanced :: Tree -> Bool
balanced (Leaf _)   = True
balanced (Node e d) = abs (size e - size d) <= 1 && balanced e && balanced d 

----------------------------------------------------------------------------------------------
--q3

data Arv a = Folha a |Nodo (Arv a) (Arv a) deriving (Show)

instance Functor Arv where
    --fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Folha n)   = Folha (f n)
    fmap f (Nodo e d) = Nodo (fmap f e) (fmap f d)












