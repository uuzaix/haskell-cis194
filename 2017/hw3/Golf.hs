module Golf where

-- 1.
everyNth :: Int -> [a] -> [a]
everyNth n xs = case drop (n -1) xs of
  (y:ys) -> y : everyNth n ys
  [] -> []

skips :: [a] -> [[a]]
skips xs = map (\n -> (everyNth n xs)) [1..(length xs)]

-- 2.
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) 
  | (y > z) && (y > x) = y : localMaxima (x:z:xs)
  | otherwise = localMaxima (x:z:xs)
localMaxima _ = []

-- 3.
countN :: [Integer] -> Integer -> Int
countN xs n = length (filter (\x -> x == n) xs)

countAllInt :: [Integer] ->[Int]
countAllInt xs = map (\n -> countN xs n) [0..9]

produceStars :: [Int] ->[String]
produceStars xs = map (\n -> (map (\x -> if x >= n then '*' else ' ') xs)) [1.. (maximum xs)]

histogram :: [Integer] -> String
histogram xs = unlines $ (reverse (produceStars (countAllInt xs))) ++ ["==========", "0123456789"]
