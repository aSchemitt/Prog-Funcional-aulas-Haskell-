{-funções recursivas sobre listas
    :  -> insere no inicio da lista
    ++ -> concatena listas
    _  -> valor não usado

--}

--produto dos elementos de uma lista
produto :: Num a => [a] -> a
produto [] = 1
produto (n:ns) = n * produto ns

--retorna o tamanho de uma lista de forma recursiva
tamanho :: [a] -> Int
tamanho [] = 0
tamanho (_:xs) = 1 + tamanho xs

--recursivamente inverte uma lista
inverte :: [a] -> [a]
inverte [] = []
--inverte recursivamente a cauda e concatena com a cabeça
inverte (x:xs) = inverte xs ++ [x]

--concatenação recursiva de duas listas
concatenar :: [a] -> [a] -> [a]
concatenar [] ys = ys
concatenar (x:xs) ys = x : concatenar xs ys


--remove os n 1ºs elementos de uma lista
remover :: Int -> [a] -> [a]
remover 0 xs = xs
remover _ [] = []
remover n (_:xs) = remover (n-1) xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort menor ++ [x] ++ quicksort maior
                    where menor = [a | a <-xs, a <= x]
                          maior = [b | b <- xs, b > x]














