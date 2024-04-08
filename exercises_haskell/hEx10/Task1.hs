module Main where
data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show

type IntTree = Tree Int



main = print (Node 1 (Leaf 2) (Leaf 3))