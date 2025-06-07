-- Ex 1.1
singletons :: [a] -> [[a]]
singletons = map (: [])

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x : xr) ys@(y : yr)
  | x < y = x : merge xr ys
  | otherwise = y : merge xs yr

mergeL :: (Ord a) => [[a]] -> [[a]]
mergeL []              = []
mergeL [xs]            = [xs]
mergeL (xs : ys : xss) = merge xs ys : mergeL xss

mergeSort :: (Ord a) => [a] -> [a]
mergeSort =
  concat
    . until (null . tail) mergeL
    . singletons

-- Ex 1.2

smartPartition :: (Ord a) => [a] -> [[a]]
smartPartition [] = []
smartPartition [x] = [[x]]
smartPartition ys = fs : smartPartition rest
  where
    (fs, rest) = partitionAux ys
    partitionAux :: (Ord a) => [a] -> ([a], [a])
    partitionAux [] = ([], [])
    partitionAux [x] = ([x], [])
    partitionAux (x : y : xs) =
      if x <= y
        then
          let (ts, sofar) = partitionAux (y : xs) in (x : ts, sofar)
        else ([x], y : xs)

mergeSort' :: (Ord a) => [a] -> [a]
mergeSort' =
  concat
    . until (null . tail) mergeL
    . smartPartition

-- Nel caso migliore (array ordinato) eseguo solo un smartPartition + concat
--  n log k dove k è il [numero sottosequenze dopo smartPartition ]

-- Ex 2.1
data BinTree a = Node a (BinTree a) (BinTree a) | Empty

data BinTree' a = Node' (BinTree' a) (BinTree' a) | Leaf a

mapBT :: (t -> a) -> BinTree t -> BinTree a
mapBT f Empty        = Empty
mapBT f (Node x u v) = Node (f x) (mapBT f u) (mapBT f v)

mapBT' :: (t -> a) -> BinTree' t -> BinTree' a
mapBT' f (Leaf a)    = Leaf (f a)
mapBT' f (Node' u v) = Node' (mapBT' f u) (mapBT' f v)

-- Ex 2.2

pRecBT :: (a -> b -> b -> b) -> b -> BinTree a -> b
pRecBT f e Empty        = e
pRecBT f e (Node x u v) = f x (pRecBT f e u) (pRecBT f e v)

pRecBT' :: (t -> t -> t) -> t -> BinTree' t -> t -- Tipo "strano", ma in realtà anche l'albero è strano (ha etichette solo nelle foglie)
pRecBT' f e (Leaf a)    = f e a
pRecBT' f e (Node' u v) = f (pRecBT' f e u) (pRecBT' f e v)

-- Ex 2.3
-- (a)
countNodes :: BinTree a -> Integer -- esplora ogni nodo una volta sola
countNodes = pRecBT (\_ y z -> 1 + y + z) 0

countNodes' :: BinTree' a -> Integer
countNodes' =
  pRecBT' ((+) . succ) 0
    . mapBT' (const 0) -- mappo prima tutto l'albero con valori 0 nei nodi, poi li conto

-- (b)
height :: BinTree a -> Integer
height =
  subtract 1
    . pRecBT (\_ y z -> succ $ max y z) 0

height' :: BinTree' a -> Integer
height' =
  subtract 1 -- conto il numero di archi (nodi - 1)
    . pRecBT' (\x y -> succ $ max x y) 0
    . mapBT' (const 0)

-- (c)
-- Scorre una volta l'albero con il p.r. + tupling

maxUnbal' :: (Ord c, Num c) => (c, c, c) -> (c, c, c) -> (c, c, c)
maxUnbal' (l, r, u) (l', r', u') = (lm, rm, um)
  where
    lm = max l r + 1
    rm = max l' r' + 1
    um = max (max u u') (abs (lm - rm))

maxUnbal :: (Ord c, Num c) => p -> (c, c, c) -> (c, c, c) -> (c, c, c)
maxUnbal _ = maxUnbal'

unbalIndx :: BinTree a -> Integer
unbalIndx =
  (\(_, _, z) -> z)
    . pRecBT maxUnbal (0, 0, 0)

unbalIndx' :: BinTree' a -> Integer
unbalIndx' =
  (\(_, _, z) -> z)
    . pRecBT' maxUnbal' (0, 0, 0)
    . mapBT' (const (0, 0, 0))

-- N-ary trees

data Tree a = K a [Tree a]

mapT :: (t -> a) -> Tree t -> Tree a
mapT f (K a t) = K (f a) (map (mapT f) t)

pRecT :: (b -> b -> b) -> a -> Tree b -> b -- Linearizza l'albero
pRecT f e (K a t) = foldr (f . pRecT f e) a t

pRecT' :: (t -> [b] -> b) -> [b] -> Tree t -> b -- principio di ricorsione
pRecT' f e (K a []) = f a e
pRecT' f e (K a t)  = f a (map (pRecT' f e) t)

countNodesT :: Tree a -> Integer
countNodesT = pRecT (+) 0 . mapT (const 1)

countNodesT' :: Tree a -> Integer
countNodesT' =
  pRecT' (flip $ (+) . sum) [0]
    . mapT (const 1)

heighT :: Tree a -> Integer
heighT =
  pRecT (max . succ) 0
    . mapT (const 0)

heighT' :: Tree t -> Integer
heighT' =
  subtract 1
    . pRecT' (flip $ (+) . maximum) [0]
    . mapT (const 1)

maxUnbalT :: (Ord c, Num c) => [(c, c, c)] -> (c, c, c)
maxUnbalT [] = undefined
maxUnbalT [(a, b, c)] = (a + 1, b + 1, c)
maxUnbalT ((h1, h1', u1) : (h2, h2', u2) : xs) = (hm, hm', um)
  where
    hm = max h1 h2 + 1
    hm' = min h1' h2' + 1
    um = max (max u1 u2) (abs (hm - hm'))

unbalIndxT :: Tree a -> Integer
unbalIndxT =
  (\(_, _, z) -> z)
    . pRecT' (const maxUnbalT) [(0, 0, 0)]

-- Ex 3.1
-- Questa è stata l'idea iniziale (Mappiamo in tutto l'albero le triple (key,up,down), poi raccogliamo
-- le key dove up = down
visita :: (Num c) => c -> BinTree c -> (c, BinTree (c, c, c))
visita up Empty = (0, Empty)
visita up (Node a u v) = (down, Node (a, up', down) u' v')
  where
    (downl, u') = visita up' u
    (downr, v') = visita up' v
    up' = a + up
    down = downr + downl + a

-- Qui ci portiamo dietro una lista e aggiungiamo sempre in testa
visita' :: (Num b, Eq b) => [b] -> b -> BinTree b -> ([b], b)
visita' xs up Empty = (xs, 0)
visita' xs up (Node a u v) = (xs'', down)
  where
    (tmpxss, downl) = visita' xs up' u
    (xs', downr) = visita' tmpxss up' v
    up' = a + up
    down = downr + downl + a
    xs'' = if up' == down then a : xs' else xs'

bimap :: (c -> a) -> (d -> b) -> (c, d) -> (a, b)
bimap f g (a, b) = (f a, g b)

visitaN :: (Eq a, Num a) => [a] -> a -> Tree a -> ([a], a) -- Inefficiente concat
visitaN xs up (K a []) = if a == up + a then (a : xs, a) else (xs, a)
visitaN xs up (K a ts) = (xs'', down)
  where
    mts = map (visitaN [] (up + a)) ts -- mapped ts
    (xs', down) = bimap concat ((+ a) . sum) $ unzip mts
    xs'' = if up + a == down then a : xs' else xs'

{- In questa versione di visita per gli alberi con branch illimitato visitiamo una sola volta ogni
nodo, e scorriamo una sola volta (durante il foldr) la lista degli adiacenti, portando in testa
la lista xs' ottenuta dopo tutte le chiamate ai sottoalberi e anche il valore down che contiente
la somma delle chiavi dei suoi sotto alberi.
NOTA : non c'è alcuna concatenazione di liste perché
grazie a foldr (e la lambda utilizzata) ad ogni esplorazione di un sotto albero riporto in cima la lista,
che successivamente utilizzo per l'esplorazione del sottoalbero successivo. In pratica è la stessa idea
degli alberi binari ma tramite foldr la posso applicare anche a quelli con branch illimitato!! -}
visitaN' :: (Eq b, Num b) => [b] -> b -> Tree b -> ([b], b) -- Lineare nel numero di nodi
visitaN' xs up (K a ts) = (xs'', down')
  where
    (xs', down) =
      foldr
        ( \t (ys, d) ->
            let (ys', d') = visitaN' ys up' t
             in (ys', d + d')
        )
        (xs, 0)
        ts
    down' = down + a
    up' = up + a
    xs'' = if up' == down' then a : xs' else xs'

-----------------------------------
nodiEquilibrati :: (Num a, Eq a) => BinTree a -> [a] -- versione inefficiente (++)
nodiEquilibrati =
  pRecBT
    (\(a, b, c) xs ys -> if b == c then a : (xs ++ ys) else xs ++ ys) -- O(|xs|) , dove |xs| < n
    []
    . snd -- O(1)
    . visita 0 -- O(n)
    -- !! VEDERE BENE LA COMPLESSITA' !! --> O(n^2)

nodiEquilibratiAux :: (Eq a) => [a] -> BinTree (a, a, a) -> [a] -- Visita una volta sola ogni nodo e computa O(1) su ognuno --> O(N) ( dove N = # nodi )
nodiEquilibratiAux xss Empty = xss
nodiEquilibratiAux xss (Node (a, b, c) u v) =
  if b == c
    then a : xss'
    else xss' -- O(1)
  where
    tmpxss = nodiEquilibratiAux xss u -- visita prima il sx
    xss' = nodiEquilibratiAux tmpxss v -- poi con la lista fatta nel sx visita il dx

nodiEquilibrati' :: (Num a, Eq a) => BinTree a -> [a] -- due visite
nodiEquilibrati' =
  nodiEquilibratiAux [] -- O(N)
    . snd -- O(1)
    . visita 0 -- O(N)
    -- Si può fare con una visita sola

nodiEquilibrati'' :: (Num a, Eq a) => BinTree a -> [a] -- una sola visita
nodiEquilibrati'' =
  fst
    . visita' [] 0 -- Conta e raccoglie

nodiEquilibratiT :: (Num a, Eq a) => Tree a -> [a] -- Ntrees inefficiente
nodiEquilibratiT =
  fst
    . visitaN [] 0

nodiEquilibratiT' :: (Num a, Eq a) => Tree a -> [a] -- Ntrees efficiente
nodiEquilibratiT' =
  fst
    . visitaN' [] 0

-------------

-- Ex.4

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
{- scanr f e = map (foldr f e) . tails
 Caso base : []
scanr f e []
={definizione scanr}  map (foldr f e) (tails [])
={definizione tails}  map (foldr f e) [[]]
={applicazione di map} [foldr f e []]
={definizione foldr}  [e]
-}

scanr' f e [] = [e]
{-
Caso induttivo : x:xs

scanr f e (x:xs)
= {definizione} map (foldr f e) (tails (x:xs))
= {def. tails} map (foldr f e) ((x:xs) : tails xs)
= {def. map} foldr f e (x:xs) : map (foldr f e) tails xs
= {ip. induttiva} foldr f e (x:xs) : scanr f e xs
= {foldr.2} f x (foldr f e xs) : scanr f e xs

Dalla definizione di scanr , sappiamo che effettua la somma prefissa a partire da destra
elemento della lista, quindi  head (scanr f e xs) = foldr f e xs

----------------------
Lemma : head (scanr f e x:xs ) = foldr f e x:xs

head (scanr f e x:xs)
= {specifica scanr} = head (map (foldr f e) tails x:xs)
= {map + tails def.} = head(foldr f e (x:xs) : map (foldr f e) tails xs)
= {head def} = foldr f e (x:xs)
----------------------

Utilizzo il lemma nel caso induttivo :

= {Lemma} f x (head (scanr f e xs)) : scanr f e xs -}

scanr' f e (x : xs) = f x (head xs') : xs'
  where
    xs' = scanr' f e xs
