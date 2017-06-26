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
