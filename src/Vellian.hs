{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Vellian
 (
 ) where

import Control.Monad.Zip
import Numeric.Natural

class Num a => IntegralDomain a

instance IntegralDomain Integer
instance IntegralDomain Float
instance IntegralDomain Double

data Fib a = Fib a a deriving
  (Show, Functor, Foldable, Traversable)

instance IntegralDomain a => Num (Fib a) where
  Fib a b + Fib c d = Fib (a + c) (b + d)
  Fib a b * Fib c d = Fib (a*(c + d) + b*c) (a*c + b*d)
  Fib a b - Fib c d = Fib (a - c) (b - d)
  negate (Fib a b) = Fib (negate a) (negate b)
  abs x = x
  signum _ = Fib 0 1
  fromInteger n = Fib 0 (fromInteger n)

instance IntegralDomain a => IntegralDomain (Fib a)

phi, unit, φ :: Fib Double
phi = φ
unit = φ
φ = Fib 1 0

instance (IntegralDomain a, Ord a) => Ord (Fib a) where
  compare (Fib a b) (Fib c d) = case compare a c of
    LT | b <= d    -> LT
       | otherwise -> go compare (a - c) (b - d)
    EQ -> compare b d
    GT | b >= d    -> GT
       | otherwise -> go (flip compare) (a - c) (b - d)
    where
      go k e f = k (sq (e + 2*f)) (5*sq e)
      sq x = x*x

instance (IntegralDomain a, Ord a) => Eq (Fib a) where
  x == y = compare x y == EQ

instance Applicative Fib where
  pure a = Fib a a
  Fib a b <*> Fib c d = Fib (a c) (b d)

instance Monad Fib where
  return a = Fib a a
  Fib a b >>= f = Fib a' b' where
    Fib a' _ = f a
    Fib _ b' = f b

instance MonadZip Fib where
  mzipWith f (Fib a b) (Fib c d) = Fib (f a c) (f b d)
  munzip (Fib (a,b) (c,d)) = (Fib a c, Fib b d)

class Num a => Additive a where
  zero :: a
  (^+^) :: a -> a -> a
  (^-^) :: a -> a -> a

instance (Num a, IntegralDomain a) => Additive (Fib a) where
  zero = Fib 0 0
  (^+^) = (+)
  (^-^) = (-)

getPhi :: Fib a -> a
getPhi (Fib a _) = a

-- | Compute the nth Fibonacci number in O(log n)
fib :: (IntegralDomain a) => Integer -> a
fib n 
    | n >= 0 = getPhi (Fib 1 0 ^ n)
    | otherwise = getPhi (Fib 1 (-1) ^ negate n)


-- | Open-Ended Fibonacci Search
--   An "upwardly closed predicate" p on the natural numbers is a predicate
--   p : Nat -> Bool for which there exists n, such that p n holds, and for all k, p k
--   implies p (succ k).
--
--   Given an upwardly closed predicate, open-ended binary searching proceeds by repeatedly
--   squaring a power of 2, until it finds an upper bound which passes the predicate, then
--   binary searching within the resulting interval.
--
--   With the machinery above it is easy to see that we can do this same thing now with
--   repeated squaring of φ to get results of form aφ + b, where a = fib(2^i),
--   b = fib(2^i - 1) until p a holds, then we have the two consecutive Fibonacci numbers
--   b and a, where b is the size of one of the two branches we want to cut a into, and a-b
--   is the other, and we know the predicate holds at a.
search :: (Natural -> Bool) -> Natural
search p 
  | p 0 = 0
  | otherwise = bound 0 1
  where
    bound !a !b
      | p b = go 0 a b
      | bb <- b*b = bound (a*a+bb) (bb+2*a*b)
    -- the answer lies in the interval (l, l+k], where i,j,k are consecutive Fibonacci numbers
    go !l !j !k
      | k == 1    = l + 1
      | p m       = go l (j - i) i
      | otherwise = go m i j
      where
        m = 1 + i
        i = k - j
