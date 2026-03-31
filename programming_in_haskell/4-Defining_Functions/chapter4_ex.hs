-- 4.8 Exercises
-- 1
halve :: [a] -> ([a],[a])
halve xs = splitAt (xsLen `div` 2) xs
  where
    xsLen = length xs

-- 2
third :: [a] -> a
third xs = head $ tail $ tail xs

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_:_:z:_) = z

-- 3
safeTail :: [a] -> [a]
safeTail xs = if null xs then [] else tail xs

safeTail' :: [a] -> [a]
safeTail' xs
  | null xs   = []
  | otherwise = tail xs

safeTail'' :: [a] -> [a]
safeTail'' []     = []
safeTail'' (x:xs) = xs

-- 8
luhnDouble :: Int -> Int
luhnDouble n
  | n2 > 9    = n2 - 9
  | otherwise = n2
    where
      n2 = n*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (luhnDouble w +
                x +
                luhnDouble y +
                z) `mod` 10 == 0
