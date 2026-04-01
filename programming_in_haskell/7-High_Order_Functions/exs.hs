-- 1
lstComp :: (a -> b) -> (a -> Bool) -> [a] -> [b]
lstComp f p xs = [f x | x <- xs, p x]

mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p xs = map f $ filter p xs

-- 2
--- a
all' :: (a -> Bool) -> [a] -> Bool
all' f = foldr (\v d -> f v && d) True

--- b
any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr (\v d -> f v || d) False

--- c
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr (\v d -> if p v then v:d else []) []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' _ []     = []
takeWhile'' p (x:xs) 
  | p x              = x:takeWhile'' p xs
  | otherwise        = []

--- d
---- Gemini
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p xs = foldr (\x f -> \xs' -> if p x then f (tail xs') else xs') (const []) xs xs

dropWhile'' :: (a -> Bool) -> [a] -> [a]
dropWhile'' _ []     = []
dropWhile'' p (x:xs)
  | p x              = dropWhile'' p xs
  | otherwise        = x:xs
