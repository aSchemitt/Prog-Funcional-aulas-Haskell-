--aula 4

fatorial :: Integer -> Integer
--assumir entrada n>=0
fatorial n  |n == 0 = 1
            |n<0 = error "n < 0 não suportado"
            |otherwise = n * fatorial (n-1)

--fatorial com pattern matching
fatorial2 :: Integer -> Integer
fatorial2 0 = 1
--fatorial2 n = n<0 = error "n < 0 não suportado"
fatorial2 n = n * fatorial2 (n-1)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

par :: Integer -> Bool
par 0 = True
par n = ???

impar :: Integer -> Bool
impar 0 = False
impar n =???


