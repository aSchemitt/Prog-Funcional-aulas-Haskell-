{-
    a função "." é a composição; recebe duas funções e retorna uma função composta

-}

somatorio :: Num a => [a] -> a
somatorio [] = 0
somatorio (x:xs) = x + somatorio xs

somatoriov2 :: Num a => [a] -> a
somatoriov2 = foldr (+) 0

tamanho :: [a] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

tamanhov2 :: [a] -> Int
tamanhov2 = foldr (\_ acc -> 1 + acc) 0








