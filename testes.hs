intercala :: [Int] -> [Int] -> [Int]
intercala [] ys = ys
intercala xs [] = xs
intercala (x:xs) (y:ys) = x:y: intercala xs ys

ultimo :: [Int] -> Maybe Int
ultimo [] = Nothing
ultimo [x] = Just x
ultimo (x:xs) = ultimo xs

ultimoO :: Int -> [Int] -> Int
ultimoO n [] = n
ultimoO n [x] = x
ultimoO n (x:xs) = ultimoO n xs


data Nat = Zero | Succ Nat deriving (Show, Eq)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1+ nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))
