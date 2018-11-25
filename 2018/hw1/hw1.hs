-- 1.
-- toDigits 1234 == [1,2,3,4]
-- toDigitsRev 1234 == [4,3,2,1]
-- toDigits 0 == []
-- toDigits (-17) == []
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | n < 10 = [n]
    | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- 2.
-- doubleEveryOther [8,7,6,5] == [16,7,12,5]
doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft (x : []) = [x]
doubleEveryOtherFromLeft (x : y : zs) = x : (y * 2): doubleEveryOtherFromLeft zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse (doubleEveryOtherFromLeft (reverse n))

--  3.
-- sumDigits [16,7,12,5] = 22
sumDigits :: [Integer] -> Integer
sumDigits n = sum ( concatMap toDigits n)

-- 4.
-- validate 4012888888881881 = True
-- validate 4012888888881882 = False
validate :: Integer -> Bool
validate n
 | mod (sumDigits (doubleEveryOther (toDigits n))) 10 == 0 = True
 | otherwise = False

--  5.
--  hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)