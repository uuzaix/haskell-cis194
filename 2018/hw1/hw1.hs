toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | n < 10 = [n]
    | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft (x : []) = [x]
doubleEveryOtherFromLeft (x : y : zs) = x : (y * 2): doubleEveryOtherFromLeft zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse (doubleEveryOtherFromLeft (reverse n))