import Control.Monad (foldM, join)
import Control.Monad.State

-- ST ---------------------------------------------
newtype ST a = S (State' -> (a, State'))

type State' = ([Int], Int)

app (S st) = st

instance Functor ST where
  fmap f st =
    S (\s -> let (x, s') = app st s in (f x, s'))

instance Applicative ST where
  pure x = S (\s -> (x, s))

  stf <*> stx =
    S
      ( \s ->
          let (f, s') = app stf s
           in let (x, s'') = app stx s'
               in (f x, s'')
      )

instance Monad ST where
  return = pure
  st >>= f =
    S
      ( \s ->
          let (x, s') = app st s
           in app (f x) s'
      )

setST :: State' -> ST ()
setST st = S (const ((), st))

getST :: ST State'
getST = S (\st -> (st, st))

-----------------------------------------------

-- Ex 1
done :: (Monad m) => m ()
done = return ()

readInt :: IO Int
readInt =
  do
    x <- getLine
    return (read x :: Int)

-- Con do notation
adder :: IO ()
adder = do
  n <- readInt
  nums <- repeatFor n readInt
  print $ sum nums
  done

-- Con bind
adder' :: IO ()
adder' =
  readInt
    >>= ( \x ->
            repeatFor' x readInt
              >>= print . sum
        )

-- implementazione replicateM
repeatFor cnt0 f =
  loop cnt0
  where
    loop cnt
      | cnt <= 0 = pure []
      | otherwise = liftA2 (:) f (loop (cnt - 1))

repeatFor' 0 _ = return []
repeatFor' n ma =
  ma
    >>= ( \x ->
            repeatFor' (n - 1) ma
              >>= \xs -> return (x : xs)
        )

-- Ex 2 ---------------------------------------------

divisors :: Integer -> [Integer]
divisors n = [d | d <- [1 .. div n 2], mod n d == 0]

divisors' n = filter ((== 0) . mod n) [1 .. div n 2]

divisorsNoSense n =
  join $
    filter ((== 0) . mod n)
      <$> ((: []) <$> [1 .. div n 2])

-- Con applicative
powerSetApp :: [a] -> [[a]]
powerSetApp [] = [[]]
powerSetApp (x : xs) = [(x :), id] <*> ps
  where
    ps = powerSetApp xs

maybeEx :: (a -> Bool) -> [a] -> Maybe a
maybeEx p [] = Nothing
maybeEx p (x : xs) =
  if p x
    then Just x
    else maybeEx p xs

isSemiPerfectM n =
  maybeEx ((== n) . sum) $
    powerSetApp $
      divisorsNoSense n

-- Ex 3 ---------------------------------------------
data BinTree a = Node a (BinTree a) (BinTree a) | Empty
  deriving (Show, Eq)

data Tree a = K a [Tree a]

nodiEquilibratiM :: (Num a, Eq a) => BinTree a -> State ([a], a) a
nodiEquilibratiM Empty = return 0
nodiEquilibratiM (Node a l r) =
  do
    (xs, up) <- get
    put (xs, up + a)
    down <- (+) <$> nodiEquilibratiM l <*> nodiEquilibratiM r
    (xs', _) <- get
    let st =
          if up == down
            then (a : xs', up)
            else (xs', up)
    put st
    return (down + a)

nodiEquilibratiM' :: (Eq a, Num a) => BinTree a -> [a]
nodiEquilibratiM' =
  fst
    . flip execState ([], 0)
    . nodiEquilibratiM

-- ST

nodiEq :: BinTree Int -> ST Int
nodiEq Empty = pure 0
nodiEq (Node a l r) =
  getST >>= \(xs, up) ->
    let upst = (xs, up + a)
     in setST upst
          *> ((+) <$> nodiEq l <*> nodiEq r)
          >>= \down ->
            getST >>= \(xs', _) ->
              let nwst =
                    if down == up
                      then (a : xs', up)
                      else (xs', up)
               in (setST nwst >> pure (down + a))

nodiEq' =
  fst
    . snd
    . flip app ([], 0)
    . nodiEq

-- Più leggibile
nodiEqDo Empty = pure 0
nodiEqDo (Node a l r) =
  do
    (xs, up) <- getST
    setST (xs, up + a)
    down <- (+) <$> nodiEqDo l <*> nodiEqDo r
    (xs', _) <- getST
    let st =
          if down == up
            then (a : xs', up)
            else (xs', up)
    setST st
    pure (down + a)

nodiEqDo' =
  fst
    . snd
    . flip app ([], 0)
    . nodiEqDo

-- alberi con branching illimitato


nodiEqT :: (Num a, Eq a) => Tree a -> State ([a], a) a
nodiEqT (K a ts) = do
  (xs, up) <- get
  put (xs, up + a)
  down <- foldr ( (<*>) . ((+) <$>) . nodiEqT) (pure 0) ts       -- oppure foldr (liftA2 (+) . nodiEqT) ... , perché liftA2 f x y = f <$> x <*> y 
  (xs', _) <- get
  let st =
        if up == down
          then (a : xs', up)
          else (xs', up)
  put st
  return (down + a)

nodiEqT' =
   fst
    . flip execState ([], 0)
    . nodiEqT


