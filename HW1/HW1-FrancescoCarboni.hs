-- Ex 1.1 -----------------------------------------------------------------------------------------
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile pred (x : xs) = if pred x then x : myTakeWhile pred xs else []
myTakeWhile _ []          = []

myDropWhile :: (t -> Bool) -> [t] -> [t]
myDropWhile pred sofar@(x : xs) = if pred x then myDropWhile pred xs else sofar
myDropWhile _ []                = []

-- Ex 1.2 -----------------------------------------------------------------------------------------
-- non ricorsiva
-- removeDupsOrd :: (Ord b) => [b] -> [b]
-- removeDupsOrd xs = if all (uncurry (==)) (zip xs (tail xs))
-- then [head xs] else map fst (filter (uncurry (/=)) (zip xs (tail xs ++ [head xs])))
-- ricorsiva
myRemoveDupsOrd :: (Eq a) => [a] -> [a]
myRemoveDupsOrd (x : xs) = x : myRemoveDupsOrd (dropWhile (== x) xs)
myRemoveDupsOrd []       = []

-- Ex 1.3 -----------------------------------------------------------------------------------------
-- utilizzo il merge sort ma un qualunque  algoritmo di ordinamento in nlogn può essere utilizzato

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x : xr) ys@(y : yr)
  | x < y = x : merge xr ys
  | otherwise = y : merge xs yr

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeSort ls `merge` mergeSort rs
  where
    (ls, rs) = split xs ([], [])
    split :: [a] -> ([a], [a]) -> ([a], [a])
    split [] sofar          = sofar
    split (a : as) (fs, ss) = split as (ss, a : fs)

-- mergesort O(nlogn) + zip O(n) + removeDupOrd' O(n) + map O(n) + mergeSort O(nlogn) + map O(n)
myRemoveDups :: (Ord b) => [b] -> [b]
myRemoveDups =
  map snd
    . mergeSort
    . map tFlip
    . myRemoveDupsOrd'
    . mergeSort
    . flip zip [1 ..]
  where
    myRemoveDupsOrd' :: (Eq a) => [(a, b)] -> [(a, b)]
    myRemoveDupsOrd' (x : xs) = x : myRemoveDupsOrd' (dropWhile (\(a, _) -> a == fst x) xs)
    myRemoveDupsOrd' [] = []
    tFlip :: (b, a) -> (a, b)
    tFlip (a, b) = (b, a)

-- Ex 2.1/2.2 -----------------------------------------------------------------------------------------
inversions :: (Ord b) => [b] -> [(b, b)]
inversions xs = [(x, x') | (x, i) <- indXs, (x', i') <- indXs, x > x' && i < i']
  where
    indXs = zip xs [0 ..]

countInversions :: (Ord a) => [a] -> Int
countInversions = length . inversions

-- Ex 2.3  -----------------------------------------------------------------------------------------
--  inv => # inversioni
-- lx , ly  => lunghezze dei sottoarray xs/ys
merge' :: (Ord a) => ([a], Int, Int) -> ([a], Int, Int) -> ([a], Int)
merge' ([], inv, lx) (ys, inv', ly) = (ys, inv + inv')
merge' (xs, inv, lx) ([], inv', ly) = (xs, inv + inv')
merge' (xs@(x : xr), inv, lx) (ys@(y : yr), inv', ly)
  | x <= y =
      let (res, inv'') = merge' (xr, inv, lx - 1) (ys, inv', ly)
       in (x : res, inv'')
  | otherwise =
      let (res, inv'') = merge' (xs, inv + lx, lx) (yr, inv', ly - 1)
       in (y : res, inv'')

mergeSort' :: (Ord a) => ([a], Int) -> ([a], Int)
mergeSort' ([], inv) = ([], inv)
mergeSort' ([x], inv) = ([x], inv)
mergeSort' (xs, inv) = (merged, invL + invR + invM)
  where
    lx = length ls
    ly = length rs
    (sortedLs, invL) = mergeSort' (ls, 0)
    (sortedRs, invR) = mergeSort' (rs, 0)
    (merged, invM) = merge' (sortedLs, 0, lx) (sortedRs, 0, ly)
    (ls, rs) = splitAt (length xs `div` 2) xs

countInversions' :: (Ord a) => [a] -> Int
countInversions' =
  snd
    . (flip . curry) mergeSort' 0

-- Ex 3.1
-- zipWith f xs ys  con zapp

zapp :: [t -> a] -> [t] -> [a]
zapp (f : fs) (x : xs) = f x : zapp fs xs
zapp _ _               = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f = zapp . map f

-- Ex 3.2

myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith' f xs ys = map (uncurry f) (zip xs ys)

-- Ex 3.3

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myMap' :: (a -> b) -> [a] -> [b]
myMap' f =
  reverse
    . foldl (flip ((:) . f)) []

-- Ex 3.4

-- Non è possibile scrivere foldr/l con map perché il suo tipo di ritorno è un [a]
-- al contrario foldr a partire da una lista resistuisce un singolo valore facendovi ricorsione
-- e combinandola tramite f a partire da un caso base .
-- map :: (a -> b) -> [a] -> [b]!!
-- foldr :: (a -> b -> b) -> b -> [a] -> b!!

-- Ex 4.1

suffissi :: [a] -> [[a]]
suffissi xs@(_ : txs) = xs : suffissi txs
suffissi []           = []

-- inefficiente ma composizionale
prefissi :: [a] -> [[a]]
prefissi =
  map reverse
    . suffissi
    . reverse

-- Ex 4.2
segments :: [a] -> [[a]]
segments = concatMap suffissi . prefissi

segSommaS :: (Num a, Eq a) => [a] -> a -> [[a]]
segSommaS xs s = filter (\x -> sum x == s) (segments xs)

-- Ex 4.3
powerList :: [a] -> [[a]]
powerList []       = [[]]
powerList (x : xs) = map (x :) (powerList xs) ++ powerList xs

sublSommaS :: (Eq a, Num a) => [a] -> a -> [[a]]
sublSommaS xs s = filter (\x -> sum x == s) (powerList xs)

---------------------------------------------------------

-- s :: (t1 -> t2 -> t3) -> (t1 -> t2) -> t1 -> t3
-- s x y z = x z (y z)

-- o :: a -> b -> a
-- o x y = x

-- circ :: (b -> c) -> (a -> b) -> a -> c
-- circ f = s (o f)

-- sommaPrec = map foldl (+) . inits  -- prefissi
-- sommaSucc = map foldr (+) . tails -- suffissi
-- Una generalizzazione sono scanl e scanr


comb k = filter ((==k).length) . powerList -- Complessità 2^n
-- (n su k ) efficiente

comb' _ 0      = [[]]
comb' [] k     = []
comb' (x:xs) k = map (x:) (comb' xs k-1) ++ comb' xs k

--anagrammi

fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)

fibsN :: Int -> [Integer]
fibsN n= take n  fibs
