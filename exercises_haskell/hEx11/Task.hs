module Main where

--  Sheep is a record that contains three elements referenced by the name, mother and father
data Sheep = Sheep {name::String, mother::Maybe Sheep, father::Maybe Sheep} deriving Show
--
-- we can use do-notation to build complicated sequences
maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = do
  m <- mother s
  father m


fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = do
  f  <- father s
  gm <- mother f
  mother gm

mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = do
  m  <- mother s
  gf <- father m
  father gf

main = print (Sheep "Shawn" Nothing Nothing)
