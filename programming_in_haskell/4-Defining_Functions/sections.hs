-- If (#) is an operator, then expressions of the form (#), (x #), and (# y) are called sections

-- Exemples
square :: Num a => a -> a
square = (^2)

square' :: Num a => a -> a
square' = \y -> y ^ 2

nthPowOf2 = (2^)
nthPowOf2 :: Integer -> Integer

nthPowOf2' :: Integer -> Integer
nthPowOf2' = \y -> 2 ^ y
