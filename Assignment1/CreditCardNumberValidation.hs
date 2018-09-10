module CreditCardNumberValidation where

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ (n `mod` 10) : []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = reverse (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs)
  | (length zs) `mod` 2 == 0 = (x * 2) : y : (doubleEveryOther zs)
  | otherwise = x : (y * 2) : (doubleEveryOther zs);

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:zs) = sum (toDigits x) + sumDigits zs

validate :: Integer -> Bool
validate n
  | sumDigits((doubleEveryOther (toDigits n))) `mod` 10 == 0 = True
  | otherwise = False
