import Prelude hiding (head, tail, last, init)
import Data.List (unfoldr)

data List a = Nil | Snoc (List a) a deriving Show

head :: List a -> a
head (Snoc Nil x) = x
head (Snoc xs  x) = head xs

last :: List a -> a
last (Snoc xs x) = x

toList :: [a] -> List a
toList = foldl Snoc Nil

fromList :: List a -> [a]
fromList = reverse . unfoldr out
  where
    out (Snoc xs x) = Just (x, xs)
    out Nil         = Nothing

fromList' :: List a -> [a]
fromList' = rec' []
  where
    rec' a Nil = a
    rec' a (Snoc xs x) = rec' (x:a) xs

class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

type Pair = (,)

instance Bifunctor Pair where
  bimap f g (x, y) = (f x, g y)

instance Bifunctor Either where
  bimap f g = either (Left . f) (Right . g)
