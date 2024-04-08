module Main where


data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show

type IntTree = Tree Int


g :: IntTree -> Int
g (Leaf v) = v
g (Node v l r) = v


f :: IntTree -> Int
f (Leaf v) = v
f (Node v l r) = g l


main = 
  if (True) then
    print (f 
      (Node 1 
        (Node 2 
          (Leaf 3) 
            (Leaf 4)
        ) 
          (Leaf 5)
      )
    )
  else
    print 4
