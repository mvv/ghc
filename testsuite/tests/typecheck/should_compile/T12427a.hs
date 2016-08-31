{-# LANGUAGE GADTs, RankNTypes #-}

-- Test pattern bindings, existentials, and higher rank

module T12427a where

data T where
  T :: a -> ((forall b. [b]->[b]) -> Int) -> T

-- Inference
-- Worked in 7.10
-- Failed in 8.0.1
h1 y = case y of T _ v -> v

-- Works in all versions
h2 :: T -> (forall b. [b] -> [b]) -> Int
h2 y = case y of T _ v -> v

-- Inference
-- Fails in 7.10  (head exploded)
-- Fails in 8.0.1 (ditto)
T _ x1 = undefined

-- Checking
-- Fails in 7.10  (head exploded)
-- Fails in 8.0.1 (ditto)
x2 :: (forall a. a->a) -> Int
T _ x2 = undefined
