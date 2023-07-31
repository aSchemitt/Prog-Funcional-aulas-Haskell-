acaov1 :: IO String
acaov1 = do
    a <- getLine
    b <- getLine
    return (a ++ b)

--main = acaov1, acaov2

acaov2 :: IO String
acaov2 = pure (++) <*> getLine <*> getLine

data Tree a = Leaf | Node (Tree a) a (Tree a ) deriving (Show)

instance Functor Tree where
    --fmap :: (a -> b) -> Tree a -> Tree b
    fmap f Leaf = Leaf
    fmap f (Node e x d ) = Node (fmap f e) (f x) (fmap f d)
{- 
instance Aplicative Tree where
    --pure :: a -> Tree a

    --<*> :: Tree(a -> b) -> Tree a -> Tree b


     -}



     data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Show)

     instance Functor Expr where
        --fmap :: (a -> b) -> Expr a -> Expr b
        fmap (Var x) = Var (f x) 
        fmap (Val n) = Val n
        fmap (Add e d) = Add (fmap f e) (fmap f d)


instance Applicative Expr where
    --pure :: a -> Expr a
    pure = Var 
    --(<*>) :: Expr (a->b) -> Expr a -> Expr b
    (Var f) <*> exp = fmap f exp 
    (Val n) <*> exp = Val n
    (Add e d) <*> exp = Add (e <*> exp) (d <*> exp)



