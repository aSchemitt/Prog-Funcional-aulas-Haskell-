


data Expr = Val Int | Div Expr Expr deriving (Show)

--função parcial
{- eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div e d) = case eval e of
                    Nothing -> Nothing
                    Just n -> case eval d of
                                Nothing -> Nothing
                                Just m -> n `divtotal` m -}
    --eval e divtotal eval d
--eval (Sub e d) = eval e - eval d

{- divtotal :: Int -> Int -> Maybe Int
divtotal _ 0 = Nothing
divtotal x y = Just (x `div` y)


eval :: Expr -> Maybe Int
eval (Val n) = pure n
eval (Div e d) = pure divtotal <*> eval e <*> eval d -}

--usando monadas
divtotal :: Int -> Int -> Maybe Int
divtotal _ 0 = Nothing
divtotal x y = Just (x `div` y)

evalm :: Expr -> Maybe Int
evalm (Val n)   = Just n
evalm (Div e d) = evalm e >>= \n -> 
                  evalm d >>= \m ->
                  n `divtotal` m 


evaldo :: Expr -> Maybe Int
evaldo  (Val n)  = Just n
evaldo (Div e d) = do
                    n <- evaldo e
                    m <- evaldo d
                    n `divtotal` m





