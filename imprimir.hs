--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

--concatena listas
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

concat' :: [a] -> [a] -> [a]
concat' [] ys = ys
concat' (x:xs) ys = x : concat' xs ys

--------------------------------------------------------------------------------------------

--retorna os divisores de um numero
divisores :: Integral a => a -> [a]
divisores n = [ x | x <-[1..n], (n `mod` x) == 0]

--verifica se é primo
prime :: Int -> Bool
prime n = divisores n == [1,n]

--retorna lista de primos até n
primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

--------------------------------------------------------------------------------------------

--retorna o tamanho de uma lista
tamanho :: [a] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

--------------------------------------------------------------------------------------------

--inverte uma lista
reverse :: [a] -> [a]
reverse = foldr (\x xs -> xs ++ [x]) []

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

--------------------------------------------------------------------------------------------

--verifica se um numero é impar
odd' :: Int -> Bool
odd' x = (not . even) x

--------------------------------------------------------------------------------------------

--função third :: [a] -> aque retorna o terceiro elemento
thirdv :: [a] -> a
thirdv xs = head (drop 2 xs)

-- versão a
third' :: [a] -> a
third' xs = head (tail (tail xs))
--versão b
third'' :: [a] -> a
third'' xs = xs !! 2
--versão c
third''' :: [a] -> a
third''' (_:_:x:_) = x

--------------------------------------------------------------------------------------------

--produz uma lista de valores repetidos. 
replicate' :: Int -> a -> [a]
replicate' n e = [e | _ <- [1..n]]

--------------------------------------------------------------------------------------------

--Uma tripla(x,y,z) de inteiros positivos é chamada pitagórica se x2+ y2= z2. 
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

--------------------------------------------------------------------------------------------

--Um inteiro positivo é dito perfeitose ele for igual a soma de todos os seus divisores
perfect :: Int -> Bool
perfect n = sum [x | x <- [1..n-1], n `mod` x == 0] == n

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]

--------------------------------------------------------------------------------------------

--O produto escalar de duas listas de inteiros
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x*y | (x,y) <- zip xs ys]

--------------------------------------------------------------------------------------------

somatoriofoldr :: Num a => [a] -> a
somatoriofoldr = foldr (+) 0

somaLista :: Num a => [a] -> a
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

--------------------------------------------------------------------------------------------

product' :: Num a => [a] -> a
product' = foldr (*) 1

--funtor
prodsv1 :: [Int] -> [Int] -> [Int]
prodsv1 xs ys = [x * y | x <- xs, y <- ys]

-- prodsv1 [1,2] [3,4]

prodsv2 :: [Int] -> [Int] -> [Int]
-- "estilo aplicativo"
prodsv2 xs ys = pure (*) <*> xs <*> ys

-- prodsv2 [1,2] [3,4]

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

-- função currificada --- expressões lambda:
mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z

mult' :: Int -> (Int -> (Int -> Int))
mult' = \x -> (\y -> (\z -> x * y * z))

--------------------------------------------------------------------------------------------

curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x,y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(x,y) -> f x y

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

-- Através de funções do preludede Haskell, defina a função 
-- halve :: [a] -> ([a],[a]) que divide uma lista de tamanho par na metade. 
-- Por exemplo:
halve' :: [a] -> ([a],[a])
halve' xs = (take n xs, drop n xs)
    where n = length xs `div` 2

halve'' :: [a] -> ([a],[a])
halve'' xs = splitAt (length xs `div` 2) xs

-- Defina a função third :: [a] -> aque retorna o terceiro elemento de 
-- uma lista que contenha no mínimo três elementos usando:

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

fatorial :: Integer -> Integer
fatorial n = product [2..n]

fatorial2 :: Integer -> Integer
fatorial2 0 = 1
fatorial2 n = n * fatorial2 (n-1)

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

--fibonacci
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

--fibonacci até n
fib :: Int -> [Int]
fib n = [fibonacci x | x <- [0..n]]

-- sequência infinita de números de fibonacci
fibs :: [Integer]
fibs = 0:1:[ x+y | (x,y) <- zip fibs(tail fibs)]

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

--MDC máximo divisor comum
mdc' :: Int -> Int -> Int
mdc' a b  | b == 0 = a
            | otherwise = mdc' b (a `mod` b)

mdc'' :: Int -> Int -> Int
mdc'' a b  = if b == 0 then a else mdc'' b (a `mod` b)

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

compareList :: Eq a => [a] -> [a] -> Bool
compareList [] [] = True
compareList [] _ = False
compareList _ [] = False
compareList (x:xs) (y:ys)   | (x == y) = compareList xs ys
                            | otherwise = False

--------------------------------------------------------------------------------------------

pertence :: Eq a => [a] -> a -> Bool
pertence [] _ = False
pertence (x:xs) n   | (x == n) = True
                    | otherwise = pertence xs n

--------------------------------------------------------------------------------------------
---verifica se uma determinada lista de números inteiros se encontra em ordem crescente
sorted :: [Int] -> Bool
sorted [] = True
sorted xs = null [ ()| (x, y) <- (zip xs(tail xs)), x > y]


sorted :: [Int] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:zs) = x <= y && sorted (y:zs)

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

--divisao segura
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

--header seguro
safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

data Expr = Val Int | Add Expr Expr | Mul Expr Expr deriving (Show)

--conta quantos valores
size :: Expr -> Int
size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

--faz o calculo
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

--------------------------------------------------------------------------------------------

--Árvore de expressões aritméticas com operação de divisão
data Expr = Val Int | Div Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

--Como Maybe é uma mônada, podemos reescrever eval usando currying e expressão lambda
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = eval x >>= \n ->
                 eval y >>= \m ->
                 safediv n m

--eval (Div (Val 10) (Val 5))
--eval (Div (Val 10) (Val 0))

evalv1 :: Expr -> Maybe Int
evalv1 (Val n) = Just n
evalv1 (Div x y) = case eval x of
    Nothing -> Nothing
    Just n -> case eval y of
        Nothing -> Nothing
        Just m -> safediv n m

--eval (Div (Val 10) (Val 5))
--eval (Div (Val 10) (Val 0))
--resultado é Nothing, que interpretamos como uma falha

--------------------------------------------------------------------------------------------

evalv2 :: Expr -> Maybe Int
evalv2 (Val n) = Just n
evalv2 (Div x y) = do
    n <- evalv2 x
    m <- evalv2 y
    safediv n m

--evalv2 (Div (Val 10) (Val 5))
--evalv2 (Div (Val 10) (Val 0))

--------------------------------------------------------------------------------------------
--resulta em uma exceção de divisão por zero
eval3 :: Expr -> Int
eval3 (Val n) = n
eval3 (Div x y) = eval x `div` eval y

--eval (Div (Val 10) (Val 5))
--eval (Div (Val 10) (Val 0))

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show,Eq)

--let arv = Node (Leaf 2) 1 (Leaf 3)
-- :t arv

--contem
contem :: Eq a => a -> Tree a -> Bool
contem x (Leaf y) = x == y
contem x (Node l y r) = x == y || contem x l || contem x r

--let arv = Node (Leaf 2) 1 (Leaf 3)
--occurs 3 arv
--occurs 0 arv

--transforma em lista
flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r
--que ordem é essa? Pre, Central ou Pos? Central
--let arv = Node (Leaf 2) 1 (Leaf 3)
--flatten arv

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

--data Tree a = Leaf a | Node (Tree a) (Tree a)
data Tree = Leaf Int | Node Tree Tree deriving (Show)
--data Tree a = Leaf a | Node(Tree a) (Tree a) deriving (Show)

--transforma em lista
flatten :: Tree -> [Int]
flatten (Leaf2 n) = [n]
leaves (Node2 e d) = leaves e ++ leaves d

--Defina uma função balanced:: Treea-> Bool que determina se uma árvore está balanceada.
--tamanho da arv
leaves :: Tree -> Int
leaves (Leaf _) = 1
leaves (Node e d) = leaves e + leaves d

--balanced :: Tree a →Bool
balanced :: Tree -> Bool
balanced (Leaf _) = True
balanced (Node e d) = abs (leaves e - leaves d) <= 1 && balanced e && balanced d

--------------------------------------------------------------------------------------------

--que converte uma lista não-vazia em uma árvore balanceada.
halve xs = splitAt (length xs `div` 2) xs

converteBalance :: [a] -> Tree a 
converteBalance [x] = Leaf x
converteBalance xs = Node (converteBalance ys) (converteBalance zs)
            where (ys,zs) = halve xs

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

data Tree a = EmptyTree | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap f EmptyTree = EmptyTree
    fmap f (Node esq x dir) = Node (fmap f esq) (f x) (fmap f dir)

--let arv = EmptyTree
--fmap (+1) arv

--let arv = Node (Node EmptyTree 2 EmptyTree) 1 (Node EmptyTree 3 EmptyTree)
--fmap (+1) arv

--let arv = Node EmptyTree "raiz" (Node EmptyTree "folha" EmptyTree)
--fmap length arv

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

-- data Tree=Leaf Int |Node Tree Treederiving (Show) como um membro da classe Functor.
--Complete a seguinte definição:

data Tree a = Leaf a | Node (Tree a) (Tree a)
instance Functor Tree where
        -- fmap :: (a -> b) -> Tree a -> Tree b
        fmap g (Leaf x)= Leaf (g x)
        fmap g (Node l r) = Node (fmap g l) (fmap g r)

--fmap even (Node (Leaf 1) (Leaf 2)

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =  qsort menores ++ [x] ++ qsort maiores
                where
                    menores = [e | e <- xs, e < x]
                    maiores = [e | e <- xs, e >= x]


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]     -- lista com um elemento apenas
msort xs = merge (msort(left)) (msort(right))
    where
        left  = take (length xs `div` 2) xs
        right = drop (length xs `div` 2) xs

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

-- Escreva um programa em Haskell que lê do console um valor inteiro e informa se ele é ou não 
-- um número Armstrong. Um número Armstrong é um número cuja soma de seus dígitos elevados à 
-- quantidade de dígitos do número resulta no próprio número. Por exemplo:
digitos :: Int -> [Int]
digitos 0 = []
digitos x = digitos (x `div` 10) ++ [x `mod` 10]

armstrong :: Int -> Bool
armstrong x = sum [ v^t | v <- xs ] == x
    where xs = digitos x
          t  = length (digitos x)

getValor :: IO Int
getValor = readLn

main = do
    putStrLn "Digite valor:"
    valor <- getValor
    putStrLn ("Resultado:" ++ show (armstrong valor))

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

--Dado o seguinte tipo para expressões aritméticasque possuem variáveis de um tipo a:
--data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving(Show)
--Mostre como tornar o tipo Exprum membro das classes Functor, Aplicativee Monad.

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Show)

instance Functor Expr where
    --fmap :: (a->b) -> Expr a -> Expr b
    fmap f (Var x) = Var (f x)
    fmap f (Val n) = Val n
    fmap f (Add e d) = Add (fmap f e) (fmap f d)

instance Applicative Expr where
    --pure :: a -> Expr a
    pure = Var
    --(<*>) :: Expr (a->b) -> Expr a -> Expr b
    (Var g) <*> ex = fmap g ex
    (Val n) <*> ex = Val n
    (Add e d) <*> ex = Add (e <*> ex) (d <*> ex)

instance Monad Expr where
    --(>>=) :: Expr a -> (a -> Expr b) -> Expr b
    (Var x) >>= g = g x
    (Val n) >>= g = Val n
    (Add e d) >>= g = Add (e >>= g) (d >>= g)


--Com a ajuda de um exemplo, explique qual o significado do operador (>>=) sobre esse tipo.
-- O operador >>= implementa o conceito de substituição de variáveis
-- em uma expressão aritmética, na qual uma variável é substituída por outra expressão.
-- let e = Add (Val 1) (Var 'x')
-- let g 'x' = Val 2
-- e >>= g

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

data Nat = Zero | Succ Nat deriving (Show, Eq)

--Zero
--Succ Zero

-- Nat para Int
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

--nat2int (Succ (Succ Zero))

--------------------------------------------------------------------------------------------

-- Int para Nat
int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

--int2nat 3

--------------------------------------------------------------------------------------------

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

--add (Succ Zero) (Succ Zero)

add' :: Nat -> Nat -> Nat
add' m n = int2nat (nat2int m + nat2int n)