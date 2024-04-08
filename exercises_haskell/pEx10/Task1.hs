data List a =  ??? | ??? deriving Show

contains :: Int -> List Int -> Bool
-- Implement the function

contains0 :: List Int -> Bool
-- Implement the function

ourmap :: (a -> b) -> List a -> List b
-- Implement the function

list1 :: List Int
list1 = Cons 1 (Cons 2 (Cons 3 Nil))

inc :: Int -> Int
inc i = i + 1

main = print (contains0 (ourmap inc list1))