--aula 5

--cauda segura, retorna a lista vazia quando recebe a lista vazia, caso contrário, retorna a cauda da lista

--com condicional
safetail :: [a] -> [a]
safetail lista = if null lista then [] else tail lista

--com guardas
safetail2 :: [a] -> [a]
safetail2 lista
        |null lista = []
        |otherwise = tail lista

--com pattern matching
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_:cauda) = cauda


--função currificada
somar :: Int -> Int -> Int
somar x y = x + y

--função currificada explícita, escrita usando lambdas (\)
somarv2 :: Int -> (Int -> Int)
somarv2 = \x -> (\y -> x + y)

inc1 :: Int -> Int
inc1 x = x + 1

--compreesor de lista pra cirar um for each 
concatena :: [[a]] -> [a]
--                  projeção | gerador1, gerador2
concatena listaDeListas = [x | lista <- listaDeListas, x <- lista]

--uso de guardas, é como um filtro
--[x | x<- [1..10], even x]

