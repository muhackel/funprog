-- Übungsblätter 1-3

module Grundlagen where
import Data.Maybe

makeittwice :: (a->a) -> a -> a
makeittwice f x = f $ f x

absolut :: Integer -> Integer
absolut x = if x < 0 then -x else x

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

factorial :: Integer ->  Integer
factorial 0 =  1
factorial x =  x * factorial (x-1)

twice :: (a->a) -> a -> a
twice f =  f.f 

flip' :: (a->b->c) -> b -> a -> c
flip' f y x = f x y


(==>) :: Bool -> Bool -> Bool
(==>) True False = False
(==>) _    _     = True


readNumber :: Char -> Maybe Integer
readNumber '0' = Just 0
readNumber '1' = Just 1
readNumber '2' = Just 2
readNumber '3' = Just 3
readNumber '4' = Just 4
readNumber '5' = Just 5
readNumber '6' = Just 6
readNumber '7' = Just 7
readNumber '8' = Just 8
readNumber '9' = Just 9
readNumber _   = Nothing

applyM :: (a->b) -> Maybe a -> Maybe b
applyM _ Nothing = Nothing
applyM f (Just x) = Just $ f x


data Nat = Zero | Succ Nat
    deriving Show


equal :: Nat -> Nat -> Bool
equal Zero Zero = True
equal _    Zero = False
equal Zero _	= False
equal (Succ x) (Succ y) = equal x y

less :: Nat -> Nat -> Bool
less Zero Zero 		= False
less Zero _    		= True
less _    Zero         	= False
less (Succ x) (Succ y) 	= less x y



