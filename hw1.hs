toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | n < 10 = [n]
  | otherwise = (mod n 10) : toDigitsRev (div n 10 )


toDigits :: Integer -> [Integer]
toDigits n = reverseList (toDigitsRev n)

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList [n] = [n]
reverseList (x : xs)= (reverseList xs) ++ [x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverseList (doubleEveryOtherRev (reverseList xs))

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x:y:xs) = x : y*2 : doubleEveryOtherRev xs 

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (concatMap toDigits xs)

validate :: Integer -> Bool
validate n 
  | ((sumDigits (doubleEveryOther (toDigits n))) `mod` 10) == 0 = True
  | otherwise = False

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a