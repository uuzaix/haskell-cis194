module Hw4 where

-- 1.
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x-2) . filter (even)

fun2' :: Integer -> Integer
fun2'  = sum . filter (even) . takeWhile (>1) . iterate (\x -> if (even x) then x `div` 2 else (x * 3 + 1)) 

--  2.
data Tree a = Leaf
              | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree as = foldr insertNode Leaf as where 
  insertNode :: a -> Tree a -> Tree a
  insertNode a Leaf = Node 0 Leaf a Leaf
  insertNode a (Node n Leaf val left) = Node (n + 1) (insertNode a Leaf) val left
  insertNode a (Node n right val Leaf) = Node (n + 1) right val (insertNode a Leaf)
  insertNode a (Node n right@(Node y _ _ _) val left@(Node y' _ _ _)) = if y < y' 
                                                                          then Node (n + 1) (insertNode a right) val left
                                                                          else Node (n + 1) right val (insertNode a left)


-- 3.
xor :: [Bool] -> Bool
xor bx = odd $ foldr countTrue 0 bx where
  countTrue :: Bool -> Int -> Int
  countTrue True n = n + 1
  countTrue _ n = n

xor' :: [Bool] -> Bool
xor' bx = foldr check False bx where
  check :: Bool -> Bool -> Bool
  check a b 
    | a == b = False
    | otherwise = True  


map' :: (a -> b) -> [a] -> [b]
-- map' f = foldr (\a b -> [f a] ++ b) []
map' f = foldr (\a -> ([f a] ++)) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)
