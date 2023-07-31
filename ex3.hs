-- aula 4

valorabs :: Int -> Int
valorabs n = if n >=0 then n else -n

sinal :: Int -> Int
sinal n = if n <0 then -1 else
            if n == 0 then 0 else 1

--guarda (|) desalinhada dá erro, são a definição de um bloco de código
--a guarda não pode começar na mesma coluna da definição da função (na mesma posição do caractere)
valorabs2 :: Int -> Int
valorabs2 n | n >= 0 = n
            |otherwise = -n

sinal2 :: Int -> Int
sinal2 n    | n < 0 = -1
            | n == 0 = 0
            |otherwise = 1