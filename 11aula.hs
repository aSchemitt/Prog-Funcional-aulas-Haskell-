

--() tupla vazia (unit) = nada; equivalente a void

--scripts que contenham qualquer função IO não são carregados

--funções com efeito colateral só podem ser chamadas na main ou a partir dela

{- 
    comando de compilação em Haskell:
    ghc --make arquivo => compilar

 -}

--main = putStrLn "Olá mundo!"

----------------

--comando "do" torna a função do tipo IO
{- 
main = do
    putStrLn "Qual seu nome?"
    nome <- getLine
    putStrLn ("Olá " ++ nome ++ "!")
 -}

 --------------------

{- 
    não tem variaveis em Haskell
    elas são o próprio valor, "nome" é o próprio retorno da função getLine
    durante a compilação, o compilador substitui "nomeUp" pelo resultado da função "map"
 -}
{- import Data.Char
main = do
    putStrLn "Qual seu nome?"
    nome <- getLine
    putStrLn "Qual seu sobrenome?"
    sobrenome <- getLine
    let nomeUp = map toUpper nome
        sobrenomeUp = map toUpper sobrenome
    putStrLn ("Olá " ++ nomeUp ++ " " ++ sobrenomeUp)
 -}

 -----------------------------------------

import Text.Printf
dobro :: Double -> Double
dobro d = d * 2

getDouble :: IO Double
getDouble = readLn

main = do
    putStrLn "Digite um valor com ponto flutuante:"
    valor <- getDouble
    printf "%.2f" (dobro valor)


