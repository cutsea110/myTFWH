module Nat where

import Prelude hiding (div,mod,divMod,sqrt)

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
  fromInteger = toNat

foldn :: (a, a -> a) -> Nat -> a
foldn (c, f) Zero = c
foldn (c, f) (Succ n) = f (foldn (c, f) n)

unfoldn :: (a -> Maybe a) -> a -> Nat
unfoldn phi x = case phi x of
  Nothing -> Zero
  Just x' -> Succ (unfoldn phi x')

fromNat :: Nat -> Int
fromNat = foldn (0, (1+))
toNat :: Integer -> Nat
toNat = unfoldn phi
  where
    phi x | x <= 0 = Nothing
          | otherwise = Just (x - 1) 

add :: Nat -> Nat -> Nat
x `add` y = foldn (x, Succ) y
mul :: Nat -> Nat -> Nat
x `mul` y = foldn (Zero, (`add` x)) y

pred' :: Nat -> Nat
pred' = unfoldn phi
  where
    phi Zero = Nothing
    phi (Succ Zero) = Nothing
    phi (Succ x)    = Just x

minus :: Nat -> Nat -> Nat
x `minus` y = foldn (x, pred') y

div :: Nat -> Nat -> Nat
x `div` y = unfoldn phi x
  where
    phi n = if n < y then Nothing else Just (n `minus` y)

mod :: Nat -> Nat -> Nat
x `mod` y = foldn (Zero, succ y) x
  where
    succ b a = if a == pred' b then Zero else Succ a

divMod :: Nat -> Nat -> (Nat, Nat)
m `divMod` n = (m `div` n, m `mod` n)

sqrt :: Nat -> Nat
sqrt n = foldn (n, (\x -> (x + n `div` x) `div` 2)) n
-- :m +Control.Arrow
-- map (fromNat.sqrt.fromInteger &&& id) [1..100]
