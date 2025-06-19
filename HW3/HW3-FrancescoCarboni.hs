type Stream a = [a]

unfold :: (b -> (a, b)) -> b -> Stream a
unfold f y = x : unfold f y' where (x, y') = f y

-- Ex 1 -------------------------------------------------------

insonnia' :: (Show a, Enum a) => a -> [Char]
insonnia' n = show n ++ " sheep " ++ rest
  where
    rest = insonnia' (succ n)

insonnia'' :: [Char]
insonnia'' = insonnia' 1

sheep :: [String]
sheep = repeat " sheep "

nats :: [Integer]
nats = [1 ..]

insonnia :: [Char]
insonnia = concat $ zipWith ((++) . show) nats sheep

-- Ex 2 -------------------------------------------------------
-- binom(n,k) = binom(n-1,k-1) + binom(n-1,k)

tartaglia :: [[Integer]]
tartaglia = [1] : nextR [0, 1]

nextR :: (Eq a, Num a) => [a] -> [[a]]
nextR xs = ts : nextR (0 : ts)
  where
    ts = nextRAux xs
    nextRAux [x] = [x]
    nextRAux (x : y : xs) = x + y : nextRAux (y : xs)

-- Ex 3 -------------------------------------------------------
filterIndex :: Integer -> [Integer] -> [Integer]
filterIndex n =
  map fst
    . filter ((/= 0) . flip mod n . snd)
    . flip zip [1 ..]

filterIndex' :: (Eq t, Num t) => t -> t -> [a] -> [a]
filterIndex' n n' (x : xs) =
  if n == n'
    then filterIndex' n 1 xs
    else x : filterIndex' n (n' + 1) xs

luckynumbersE :: (Eq a, Num a) => a -> a -> [a] -> [a]
luckynumbersE ls start ss = l : ls'
  where
    ss' = tail $ filterIndex' ls start ss
    l = head ss'
    ls' = luckynumbersE l (start + 1) ss'

luckynumbersIV :: [Integer]
luckynumbersIV = 1 : 3 : luckynumbersE 3 2 [3, 5 ..]

minus :: (Eq a) => [a] -> [a] -> [a]
minus [] ys = ys
minus xs'@(x : xs) ys'@(y : ys)
  | x == y = minus xs ys
  | otherwise = ys

luckynumbers' :: Integer -> [Integer] -> [Integer] -> [Integer]
luckynumbers' n ss ls = l : ls'
  where
    ss' = filterIndex n ss
    l = head $ minus ls ss'
    ls' = luckynumbers' l ss' (ls ++ [l])

luckynumbers :: [Integer]
luckynumbers = 1 : 3 : luckynumbers' 3 [1, 3 ..] [1, 3]

lastLucky :: (Eq t) => t -> [t] -> t
lastLucky x (y : ys)
  | x == y = head ys
  | otherwise = lastLucky x ys

luckynumbers'' :: Integer -> [Integer] -> [Integer]
luckynumbers'' n ss = l : ls'
  where
    ss' = filterIndex n ss
    l = lastLucky n ss'
    ls' = luckynumbers'' l ss'

luckynumbers''' :: [Integer]
luckynumbers''' = 1 : 3 : luckynumbers'' 3 [1, 3 ..] -- evita concatenazione di liste e non porta dietro un parametro inutile

-- Ex 4 -------------------------------------------------------

data Nat = Z | S Nat
  deriving (Show, Eq, Ord)

primRec :: (Nat -> a -> a) -> a -> Nat -> a
primRec h g Z = g
primRec h g (S n) = h n (primRec h g n)

iter :: (a -> a) -> a -> Nat -> a
iter h a Z = a
iter h a (S n) = h $ iter h a n

id' :: p -> p
id' x = x

for :: (b -> b) -> Nat -> b -> b
for h Z = id'
for h (S n) = h . for h n

-- for
primRec'' :: ((Nat, b) -> b) -> b -> Nat -> b
primRec'' h a n =
  snd $
    for (\(c, d) -> (S c, h (c, d))) n (Z, a)

-- Iter
primRec' :: ((Nat, b) -> b) -> b -> Nat -> b
primRec' h a =
  snd
    . iter (\(c, d) -> (S c, h (c, d))) (Z, a)

-- churchN h a = h(h(...h(a)))  n volte, un numero di church ha lo stesso tipo di un iteratore
-- quindi possiamo utilizzarlo proprio come fatto sopra con il for (è come se stessi usando iter per creare primRec)

-- Ackermann
{-- A(0, n) = n+1
A(m+1, 0) = A(m, 1)
A(m+1, n+1) = A(m, A(m+1, n)) --}

pr :: (Eq a, Num a, Enum a) => (a -> b -> b) -> b -> a -> b
pr h g 0 = g
pr h g n = h (pred n) (pr h g (pred n))

ackermann :: (Eq a, Num a, Enum a) => a -> a -> a
ackermann =
  pr
    ( \_ f n ->
        pr (\_ h -> f h) (f 1) n
    )
    succ

-- Ex 5 -------------------------------------------------------

data BinTree a = Node a (BinTree a) (BinTree a) | Empty
  deriving (Show)

cwTreeGen :: (Num b) => b -> b -> BinTree (b, b)
cwTreeGen m n = Node (m, n) l r
  where
    l = cwTreeGen m (m + n)
    r = cwTreeGen (m + n) n

cwTree :: BinTree (Integer, Integer)
cwTree = cwTreeGen 1 1

takeNlevels :: (Eq t, Num t) => t -> BinTree a -> BinTree a
takeNlevels 0 b = Empty
takeNlevels n (Node q l r) = Node q (takeNlevels (n - 1) l) (takeNlevels (n - 1) r)

-- versione meno efficiente
visitaLivelli' :: (Eq a) => BinTree a -> [a]
visitaLivelli' = reverse . bfs 0 []

bfs :: (Num t, Eq t, Eq a) => t -> [a] -> BinTree a -> [a]
bfs _ _ Empty = []
bfs 0 xs b@(Node a l r) = bfs 1 (a : xs) b
bfs n xs b = if head xs /= head xs' then bfs (n + 1) xs' b else xs
  where
    xs' = bfsAux n xs b
    bfsAux :: (Eq p, Num p, Eq a) => p -> [a] -> BinTree a -> [a]
    bfsAux n xs Empty = xs
    bfsAux 0 xs (Node a _ _) = a : xs
    bfsAux n xs (Node a l r) = xs'
      where
        tmpxs = bfsAux (n - 1) xs l
        xs' = bfsAux (n - 1) tmpxs r

-- "Bfs" (per alberi)  più efficiente, mantiene una lista di sottoalberi da espandere, invece di ripercorrere
-- ogni volta tutto l'albero.
bfs' :: [BinTree b] -> [b] -> [b]
bfs' [] xs = xs
bfs' fs xs =
  let (fs', xs'') = foldr (flip $ uncurry bfsAux') ([], xs) fs
   in bfs' fs' xs''
  where
    bfsAux' :: [BinTree a] -> [a] -> BinTree a -> ([BinTree a], [a])
    bfsAux' fs xs Empty = (fs, xs)
    bfsAux' fs xs (Node a l r) = (r : l : fs, a : xs)

visitaLivelli :: BinTree b -> [b]
visitaLivelli b = reverse $ bfs' [b] []
