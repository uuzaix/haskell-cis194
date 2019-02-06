module Golf where

-- ex.1
skips :: [a] -> [[a]]

everyNth ::  [a] -> Int -> [a]
everyNth xs n = case (drop (n - 1) xs ) of
    (y:ys) -> y : (everyNth ys n)
    [] -> []

skips xs = map (everyNth xs) [1 .. length xs]

-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
    -- skips [True,False] == [[True,False], [False]]
-- skips [] == []


-- ex.2
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
    | x < y && y > z = y : localMaxima(z:xs)
    | otherwise = localMaxima(y:z:xs)
localMaxima _ = []

-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []

--ex.3
countInt :: Integer -> [Integer] -> Int
countInt n xs = length (filter (\x -> x == n) xs)

countAllInt :: [Integer] -> [Int]
countAllInt xs = map (\n -> countInt n xs ) [0..9]

produceStars :: [Int] -> [String]
produceStars xs = map (\n -> map (\x -> if x >= n then '*' else ' ') xs) [1..(maximum xs)]

histogram :: [Integer] -> String
histogram xs = unlines (reverse (produceStars (countAllInt xs)) ++ ["==========", "0123456789"])


