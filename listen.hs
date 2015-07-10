module Listen where

import Data.Maybe

data Nat = Zero | Succ Nat
    deriving Show
    
main :: IO ()
main = putStrLn $ show $ "linsert 8 [1,2,3]"


gennat :: Integer -> Nat
gennat 0 = Zero
gennat x = rgennat Zero x

rgennat :: Nat -> Integer -> Nat
rgennat x 0 = x
rgennat x n = rgennat (Succ x) (n-1)

foldN :: a -> (a -> a) -> Nat -> a
foldN c f Zero = c
foldN c f (Succ n) = f $ foldN c f n

nat2int :: Nat -> Integer
nat2int = foldN 0 (+1)

plusN :: Nat -> Nat -> Nat
plusN n = foldN n Succ

--data List a = Nil | Val a (List a)
--data [a] = [] | (a:[a])

lcat :: [a] -> [a] -> [a]
lcat [] ys = ys
lcat (x:xs) ys = x : (lcat xs ys) 

lmap :: (a -> a) -> [a] -> [a]
lmap _ [] = []
lmap f (x:xs) = (f x) : (lmap f xs)

llength :: [a] -> Integer
llength [] = 0
llength (x:xs) = 1 + (llength xs)

lfoldr :: (a -> b -> b) -> b -> [a] -> b
lfoldr f c [] = c
lfoldr f c (x:xs) = f x (lfoldr f c xs)

lsum :: [Integer] -> Integer
lsum = lfoldr (+) 0 

--linsertpermute :: a -> [a] -> [[a]]
--linsertpermute c [] = [[c]]
--linsertpermute c (x:xs) = [c :  (x:xs)] ++ (linsertpermute c xs)

