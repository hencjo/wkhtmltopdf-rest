module Validation (
    AccumulatedValidation (..),
    Validation (..),
    (<#>)
) where

--import Control.Applicative

data AccumulatedValidation f = Success f | Failure [String] deriving (Show, Eq)
data Validation a = Invalid String | Valid a deriving (Show, Eq)

(<#>) :: AccumulatedValidation (a -> b) -> Validation a -> AccumulatedValidation b
(Success f)  <#> (Valid a)     = Success (f a)
(Success _)  <#> (Invalid msg) = Failure [msg]
(Failure xs) <#> (Valid _)     = Failure xs
(Failure xs) <#> (Invalid msg) = Failure (xs ++ [msg])

{-
instance Functor AccumulatedValidation where
    fmap f (Success a)  = Success (f a)
    fmap _ (Failure xs) = Failure xs

instance Applicative AccumulatedValidation where
    pure a                        = Success a
    (Success f)  <*> (Success a)  = Success (f a)
    (Success _)  <*> (Failure ys) = Failure ys
    (Failure xs) <*> (Failure ys) = Failure (xs ++ ys)
    (Failure xs) <*> (Success _)  = Failure xs

instance Monad AccumulatedValidation where
    (Success a)  >>= f = f a
    (Failure xs) >>= _ = Failure xs
    return a           = Success a
    -}

instance Functor Validation where
    fmap f (Valid a)   = Valid (f a)
    fmap _ (Invalid m) = Invalid m
