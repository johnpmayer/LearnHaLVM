
{-# OPTIONS -Wall #-}

module Queue where

data Queue a = Q { front :: [a], back :: [a] }

empty :: Queue a
empty = Q [] []

enqueue :: a -> Queue a -> Queue a
enqueue x q = q { back = x : back q }

dequeue :: Queue a -> (Queue a, Maybe a)
dequeue q@(Q [] []) = (q, Nothing)
dequeue (Q (x:xs) _) = (Q xs [], Just x)
dequeue (Q [] ys) = 
    (Q (tail sy) [], Just $ head sy) where
        sy = reverse ys

