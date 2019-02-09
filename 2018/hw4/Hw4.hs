module Hw4 where

-- ex.1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
-- fun1' = product . map(\x -> x - 2) . filter(even)
fun1' = product . map(subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate f
    where f n = if even n then n `div` 2 else 3*n + 1

-- ex.2
data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr f Leaf
f :: a -> Tree a-> Tree a
f n Leaf  = Node 0 Leaf n Leaf
f n (Node 0 Leaf v Leaf)  = Node 1 (f n Leaf) v Leaf
f n (Node x l1@(Leaf) v t2)  = Node x (f n l1) v t2
f n (Node x t1 v l2@(Leaf))  = Node x t1 v  (f n l2)
f n (Node x t1@(Node x1 t11 v1 t12) v t2@(Node x2 t21 v2 t22))
            | x1 > x2  = Node x t1 v (f n t2)
            | x1 == x2  = Node (x+1) t1 v (f n t2)
            | otherwise = Node x (f n t1) v t2

-- Node 5
--     (Node 2
--         (Node 0 Leaf 'F' Leaf)
--         'I'
--         (Node 1
--             (Node 0 Leaf 'B' Leaf)
--             'C'
--             Leaf))
--     'J'
--     (Node 2
--         (Node 1
--             (Node 0 Leaf 'A' Leaf)
--             'G'
--             Leaf)
--         'H'
--         (Node 1
--             (Node 0 Leaf 'D' Leaf)
--             'E'
--             Leaf))
-- foldr :: (a -> b -> b) -> b -> t a -> b