


type Posicao = (Int,Int)

origem :: Posicao
origem = (0,0)

projecaox :: Posicao -> Int
projecaox (x,y) = x

projecaoy :: Posicao -> Int
projecaoy (x,y) = y


type Par a = (a,a)

montaPar :: a -> Par a
montaPar x = (x,x)



---

data Movimento = Norte | Sul | Leste | Oeste deriving (Show)

move :: Movimento -> Posicao -> Posicao
move Norte (x,y) = (x,y+1)
move Sul   (x,y) = (x,y-1)
move Leste (x,y) = (x+1,y)
move Oeste (x,y) = (x-1,y)


moves :: [Movimento] -> Posicao -> Posicao
moves [] p = p
moves (m:ms) p = moves ms (move m p)

moveL :: [Movimento] -> Posicao -> Posicao
moveL [] p = p
moveL (m:ms) p = moveL ms (move m p)

--data Pos = Int Int

data Shape = Circulo Float | Retangulo Float Float deriving (Show)

area :: Shape -> Float
area (Circulo r) = pi*r^2
area (Retangulo b a) = b*a  


--tipos recursivos

data Nat = Zero | Succ Nat deriving (Show)

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

somar :: Nat -> Nat -> Nat
somar Zero m     = m
somar (Succ n) m = Succ (somar n m) 






