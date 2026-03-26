module ExtraProblems where

import Data.Numbers.Primes

alternate :: Num a => [a] -> a
alternate []     = 0
alternate xs     = aux xs 1
  where
    aux [] _         = 0
    aux (x:xs') sig  = x*sig + aux xs' (sig*(-1))

-- Do not pass a empty list please
min_max :: Ord a => [a] -> (a,a)
min_max (x:[]) = (x, x)
min_max (x:xs) = aux xs (x,x)
  where
    aux [] acc       = acc
    aux (x':xs') acc =
      case acc of
        (min', max') | x' < min' -> aux xs' (x', max')
                     | x' > max' -> aux xs' (min', x')
                     | otherwise -> aux xs' acc

cumsum :: Num a => [a] -> [a]
cumsum [] = []
cumsum xs = aux xs 0 
  where
    aux [] acc      = []
    aux (x:xs') acc = (x+acc):aux xs' (x+acc)

-- Lists must be the same length
repeat' :: [a] -> [Integer] -> [a]
repeat' [] ys         = []
repeat' xs []         = xs
repeat' (x:xs) (y:ys) = [x | _ <- [1..y]] ++ repeat' xs ys

repeat'' :: [a] -> [Integer] -> [a]
repeat'' [] ys         = []
repeat'' xs []         = xs
repeat'' (x:xs) (y:ys) = generate x y ++ repeat'' xs ys
  where
    generate x' y'
      | y' == 0   = []
      | otherwise = x:generate x' (y'-1)

hasTrue :: [Bool] -> Bool
hasTrue []     = False
hasTrue (b:bs)
  | b          = True
  | otherwise  = hasTrue bs

allTrue :: [Bool] -> Bool
allTrue []     = True
allTrue (b:bs)
  | b          = allTrue bs
  | otherwise  = False

zip' :: [a] -> [b] -> [(a,b)]
zip' xs ys =
  case (xs, ys) of
    ([], [])     -> []
    (_, [])      -> []
    ([], _)      -> []
    (x:xs, y:ys) -> (x,y):zip' xs ys

zipRecycle :: [a] -> [b] -> [(a,b)]
zipRecycle xs ys
  | xsLen == 0 || ysLen == 0 = []
  | xsLen == ysLen           = zip xs ys
  | xsLen > ysLen            = zipRecycle xs ysPadded
  | ysLen > xsLen            = zipRecycle xsPadded ys
  where
    xsLen = length xs
    ysLen = length ys
    ysPadded = ys ++ take (xsLen - ysLen) ys
    xsPadded = xs ++ take (ysLen - xsLen) xs

splitup :: (Ord a, Num a) => [a] -> ([a], [a])
splitup []    = ([],[])
splitup (x:xs)
  | x <= 0    = (x:neg, nonNeg)
  | otherwise = (neg, x:nonNeg)
  where
    (neg, nonNeg) = splitup xs

splitat :: Ord a => [a] -> a -> ([a], [a])
splitat [] t   = ([],[])
splitat (x:xs) t
  | x <= t     = (x:blw, abv)
  | otherwise  = (blw, x:abv)
  where
    (blw, abv) = splitat xs t

-- Check if a list is sorted in increasing order
isSorted :: Ord a => [a] -> Bool
isSorted []     = True
isSorted (x:[]) = True
isSorted (x:y:xs)
  | x > y       = False
  | otherwise   = isSorted (y:xs)

-- Check if a list is sorted in any order
isAnySorted :: Ord a => [a] -> Bool
isAnySorted xs = checkSort xs True True
  where
    checkSort [] _ _        = True
    checkSort (x:[]) _ _    = True
    checkSort (x:y:xs') inc dec
      | inc && x > y  = checkSort (x:y:xs') False dec
      | inc && x <= y = checkSort (y:xs') inc False
      | dec && x < y  = checkSort (x:y:xs') inc False
      | dec && x >= y = checkSort (y:xs') False dec
      | otherwise     = False

-- Expects 2 sorted lists (increasing order)
sortedMerge :: Ord a => [a] -> [a] -> [a]
sortedMerge [] []         = []
sortedMerge [] ys         = ys
sortedMerge xs []         = xs
sortedMerge (x:xs) (y:ys) 
  | x > y     = y:sortedMerge (x:xs) ys
  | otherwise = x:sortedMerge xs (y:ys)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:[]) = [x]
qsort (x:xs) = sortedMerge (qsort blw) (x:qsort abv)
  where
    (blw, abv) = splitat xs x

divide :: [a] -> ([a],[a])
divide xs = aux xs True
  where
    aux [] _         = ([],[])
    aux (x:xs) isLft
      | isLft        = (x:lft, rght)
      | otherwise    = (lft, x:rght)
      where
        (lft,rght)   = aux xs (not isLft)

notSoQuickSort :: Ord a => [a] -> [a]
notSoQuickSort []     = []
notSoQuickSort (x:[]) = [x]
notSoQuickSort xs     = sortedMerge (notSoQuickSort lft) (notSoQuickSort rght)
  where
    (lft, rght) = divide xs

fullDivide :: Integer -> Integer -> (Integer, Integer)
fullDivide k n = aux 0 n
  where
    aux count curr
      | curr `mod` k == 0 = aux (count + 1) (curr `div` k)
      | otherwise         = (count, curr)

factorize :: Integer -> [(Integer, Integer)]
factorize n
  | n < 2     = []
  | otherwise = check_primes n primes'
  where
    threshold = floor (sqrt (fromInteger n))
    primes'   = takeWhile (<= threshold) primes
    
    check_primes n' []
      | n' /= 1            = [(n',1)]
      |otherwise           = []
    check_primes n' (p:ps) 
      | times /= 0         = (p,times):check_primes rest ps
      | otherwise          = check_primes n' ps
        where
          (times, rest)    = fullDivide p n'

multiply :: [(Integer, Integer)] -> Integer
multiply []         = 1
multiply ((p,e):xs) = p^e * multiply xs

allDivisors :: [(Integer, Integer)] -> [Integer]
allDivisors [] = [1]
allDivisors ps = getDivisors [] (unpairFactorsList ps) 1
  where
    unpairFactorsList []          = []
    unpairFactorsList ((p,e):ps') =
      [[power | x <- [0..e], let power = p ^ x]] ++ unpairFactorsList ps' 

    getDivisors [] [] _            = []
    getDivisors [] (ms':mss) d     = getDivisors ms' mss d
    getDivisors (m:[]) (ms':mss) d = getDivisors ms' mss (d*m)
    getDivisors (m:ms) [] d        = m*d:getDivisors ms [] d 
    getDivisors (m:ms) (ms':mss) d = sortedMerge (getDivisors ms (ms':mss) d) (getDivisors ms' mss (m*d))
