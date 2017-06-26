module Nat where

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
  (+) = add
  (*) = mul
  abs = id
  signum = foldn (Zero, const (Succ Zero))
  (-) = minus
  fromInteger x | x <= 0 = Zero
                | otherwise = Succ (fromInteger (x - 1))

fromNat :: Nat -> Int
fromNat = foldn (0, (1+))

foldn :: (a, a -> a) -> Nat -> a
foldn (c, f) Zero = c
foldn (c, f) (Succ n) = f (foldn (c, f) n)

unfoldn :: (a -> Maybe a) -> a -> Nat
unfoldn phi x = case phi x of
  Nothing -> Zero
  Just x' -> Succ (unfoldn phi x')

add :: Nat -> Nat -> Nat
x `add` y = foldn (x, Succ) y
mul :: Nat -> Nat -> Nat
x `mul` y = foldn (Zero, (`add` x)) y

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

-- 漸化式 a(n+1) = f(a(n)), a(0) = c
-- の一般解がfoldn (c,f)
sqrtn :: Nat -> Nat
sqrtn n = foldn (n, (\x -> (x + n `div'` x) `div'` 2)) n

divMod' :: Nat -> Nat -> (Nat, Nat)
m `divMod'` n | m < n = (Zero, m)
              | otherwise = (Succ q, r)
  where
    (q, r) = divMod' (m - n) n
