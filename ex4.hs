--aula 4

--calcular o maior entre dois numeros inteiros
maiorInt :: Integral a => a -> a -> a
maiorInt x y | x >= y = x
          | otherwise = y

--calcular o maior entre dois numeros reais
maiorReal :: Real a => a -> a -> a
maiorReal x y | x >= y = x
          | otherwise = y

--pattern matching
neg :: Bool -> Bool
neg True = False
neg False = True

e :: Bool -> Bool -> Bool
e True True = True
e True False = False 
e False True = False
e False False = False

e' :: Bool -> Bool -> Bool
e' True True = True
e' _  _ = False

e'' :: Bool -> Bool -> Bool
e'' True y = y

--pattern matching sobre listas
cabeca :: [a] -> a 
cabeca (x:_) = x
cabeca [] = error "[] não suportado"

cauda :: [a] -> [a]
cauda (_:xs) = xs


