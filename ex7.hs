--aula 6

-- list comprehenson
-- [projeção | gerador1, ..., geradorN, guarda]
--                                      filtro

--retorna os todos os pares de elementos de uma lista
--função 'zip' retorna uma lista de pares de elementos nos índices de duas listas
--pareia a lista com a cauda dela
pares :: [a] -> [(a,a)]
pares xs = zip xs (tail xs)

--pra saber se uma lista está ordenada, percorre todos os pares
--e testa a ordem de cada um
--usa a operação 'and' para verificar se a lista resultante de x <= y possui apenas True
ordenada :: Ord a => [a] -> Bool
ordenada xs = and [ x <= y | (x,y) <- pares xs]

--dado um elemento e uma lista, retorna uma lista com os índices de onde o elemento se encontra na lista de entrada
--type class Eq implementa '=='
--obtem o par (elemento, índice) através da função 'zip' com uma lista infinita
--retorna uma lista dos índices se o elemento do par for igual ao elemento de entrada 
posicoes :: Eq a => a -> [a] -> [Int]
posicoes x xs = [(i) | (e,i) <- zip xs [0 ..], x == e]

--o mesmo da anterior, mas retorna o par (elemento, índice)
posicoes2 :: Eq a => a -> [a] -> [(a,Int)]
posicoes2 x xs = [(e,i) | (e,i) <- zip xs [0 ..], x == e]

--conta quantas vezes um caractere aparece na string
--filtra a string criando uma lista de caracteres iguais
--calcula o tamanho da nova lista
contar :: Char -> String -> Int
contar c s = length [x | x <- s, x == c]


--retorna todos os numeros pitagoricos entre 1 e n
--pitagorcos são a combinação de 3 números que satisfaçam a expressão "x^2 + y^2 = z^2" 
pitagorico :: Int -> [(Int, Int, Int)]
pitagorico n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]


--verifica se um número é perfeito
perfeito :: Int -> Bool
perfeito n =sum [x | x <- [1..n-1], mod n x == 0] == n

--nro perfeitos até n
--retorna uma lista com todos os nros perfeitos
perfeitos :: Int -> [Int]
perfeitos n = [x | x <- [1..n], perfeito x]

--produto escalar de duas listas
--cria um par de valores dos elementos de cada lista
--multiplica o par e soma os resultados
escalar :: [Int] -> [Int] -> Int
escalar xs ys = sum [x * y | (x,y) <- zip xs ys]


