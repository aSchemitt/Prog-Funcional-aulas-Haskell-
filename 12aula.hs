--funtores

inc :: [Int] -> [Int]
inc = fmap (+1)

incGeral :: Functor f => f Int -> f Int
incGeral = fmap (+1)

--------------
{- 
main = do
    putStrLn "Digite uma frase:"
    fraseReversa <- fmap (reverse) getLine
    putStrLn ("Resultado: "++ fraseReversa)
     -}

----------------
data ArvBin a = VaziaBin | Node (ArvBin a) a (ArvBin a ) deriving (Show)

instance Functor ArvBin where
    -- fmap :: (a->b) -> ArvBin a -> ArvBin b
    fmap f VaziaBin = VaziaBin
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)






