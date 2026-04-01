-- 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- 3
pow :: Integral a => a -> a -> a
pow 0 _ = 0
pow _ 0 = 1
pow b e = b * pow b (e-1)

-- 4
euclid :: Int -> Int -> Int
euclid 0 b = b
euclid a 0 = a
euclid a b
  | a > b     = euclid b (a `rem` b)
  | a == b    = a
  | otherwise = euclid a (b `rem` a)

-- 6
--- a
and' :: [Bool] -> Bool
and' []     = True
and' (b:bs) = b && and' bs

--- b
concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ concat' xss

--- c
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n v = v:replicate' (n-1) v
