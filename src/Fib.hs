{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Fib where

import           Control.Monad.Zip
import           StarSemiring

data Fib a = Fib a a
  deriving (Show, Functor, Foldable, Traversable)

instance (Num a, Semiring a) => Semiring (Fib a) where
  zero = Fib 0 0
  (Fib a b) <+> (Fib c d) = Fib (a <+> c) (b <+> d)
  one  = Fib 1 0
  (Fib a b) <.> (Fib c d) = Fib (a <.> (c <+> d) <+> b <.> c) (a <.> c <+> b <.> d)

instance (Num a, Semiring a) => StarSemiring (Fib a) where
  star (Fib a b) = Fib b (a - b - 1)

instance (Num a, StarSemiring a) => KleeneAlgebra (Fib a) where

phi, unit, φ :: Fib Double
phi = φ
unit = φ
φ = Fib 1 0

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


