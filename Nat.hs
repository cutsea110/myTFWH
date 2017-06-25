import Prelude hiding ((^^),div)

(^^) :: (Fractional a, Integral b) => a -> b -> a
x ^^ y | y >= 0 = x^y
       | otherwise = recip $ x^(abs y)

div :: Integral a => a -> a -> a
div x y = floor (fromIntegral x / fromIntegral y)

main :: IO ()
main = undefined

leq :: Integer -> Float -> Bool
x `leq` y = fromInteger x <= y
lt :: Float -> Integer -> Bool
x `lt` y = x < fromInteger y

type Interval = (Integer, Integer)

shrink :: Float -> Interval -> Interval
shrink x (m, n) = if (p^2) `leq` x then (p, n) else (m, p)
  where
    p = choose (m, n)

choose :: Interval -> Integer
choose (m, n) = (m + n) `div` 2

bound :: Float -> Interval
bound x = (0, upper x)

upper :: Float -> Integer
upper x = until ((x `lt`).(^2)) (* 2) 1

isqrt :: Float -> Integer
isqrt x = fst (until unit (shrink x) (bound x))
  where
    unit (m, n) = m + 1 == n

sqrt' :: Float -> Float
sqrt' x = until goodEnough improve 1
  where
    goodEnough y = abs (y^^2 - x) <= 2 * epsilon * x
    improve y = (y + x / y) / 2
    machineEpsilon = until (\x -> 1 + x == 1) (/2) 1
    epsilon = 2 * machineEpsilon :: Float

data Nat = Zero | Succ Nat deriving Show

instance Eq Nat where
  Zero   == Zero   = True
  Succ _ == Zero   = False
  Zero   == Succ _ = False
  Succ m == Succ n = m == n

instance Ord Nat where
  Zero   <= Zero   = True
  Zero   <= Succ _ = True
  Succ _ <= Zero   = False
  Succ m <= Succ n = m <= n

instance Num Nat where
  m + Zero = m
  m + (Succ n) = Succ (m + n)

  m * Zero = Zero
  m * (Succ n) = m * n + m

  abs n = n
  signum Zero = Zero
  signum (Succ n) = Succ Zero

  m - Zero = m
  Zero - Succ n = Zero
  Succ m - Succ n = m - n

  fromInteger x | x <= 0 = Zero
                | otherwise = Succ (fromInteger (x -1))

toNat :: Int -> Nat
toNat n | n <= 0 = Zero
        | otherwise = Succ (toNat (n - 1))
fromNat :: Nat -> Int
fromNat = foldn (0, (1+))

foldn :: (a, a -> a) -> Nat -> a
foldn (c, f) Zero = c
foldn (c, f) (Succ n) = f (foldn (c, f) n)

unfoldn :: (a -> Maybe a) -> a -> Nat
unfoldn phi x = case phi x of
  Nothing -> Zero
  Just x' -> Succ (unfoldn phi x')

plus :: Nat -> Nat -> Nat
x `plus` y = foldn (x, Succ) y

pred' :: Nat -> Nat
pred' Zero = Zero
pred' (Succ n) = n

minus :: Nat -> Nat -> Nat
x `minus` y = foldn (x, pred') y

div' :: Nat -> Nat -> Nat
x `div'` y = unfoldn phi x
  where
    phi n = if n < y then Nothing else Just (n `minus` y)

mod' :: Nat -> Nat -> Nat
x `mod'` y = foldn (Zero, succ y) x
  where
    succ b a = if a == pred' b then Zero else Succ a

divMod' :: Nat -> Nat -> (Nat, Nat)
m `divMod'` n | m < n = (Zero, m)
              | otherwise = (Succ q, r)
  where
    (q, r) = divMod' (m - n) n
