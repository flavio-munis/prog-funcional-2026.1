import Data.Array
import Data.Char

-- 1
nFirstSquares :: Int -> [Int]
nFirstSquares n = [x^2 | x <- [1..n]]

-- sum $ nFirstSquares 100

-- 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- 3
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- 4
replicate' :: Int -> a -> [a]
replicate' n a = [a | _ <- [1..n]]

-- 5
pythsNaive :: Int -> [(Int, Int, Int)]
pythsNaive n = [ (x,y,z) |
                 x <- [1..n],
                 y <- [1..n],
                 z <- [1..n],
                 z > x && z > y && x^2 + y^2 == z^2]
          
pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (x,y,z) |
            z <- [1..n],
            x <- [1..z],
            y <- [1..z],
            x^2 + y^2 == z^2]

--- A little sauce with memoization
---- Bad sauce: For large N because List access time is O(n)
pythsMemoNaive :: Int -> [(Int, Int, Int)]
pythsMemoNaive n = [ (x,y,z) |
                     z <- [1..n],
                     x <- [1..z],
                     y <- [1..z],
                     powMemo !! (x-1) + powMemo !! (y-1) == powMemo !! (z-1)]
  where
    powMemo = [x^2 | x <- [1..n]]

---- Good Sauce: Arrays have O(1) access time
pythsMemo :: Int -> [(Int, Int, Int)]
pythsMemo n = [ (x,y,z) |
                z <- [1..n],
                x <- [1..z],
                y <- [1..z],
                powMemo ! x + powMemo ! y == powMemo ! z]
  where
    powMemo = array (1,n) [(x, x^2) | x <- [1..n]]

-- 6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum $ factors x) - x == x]

-- 7
--- eq == eq' = True
eq = [(x,y) | x <- [1,2], y <- [3,4]]
eq' = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

-- 8
find :: Eq a => a -> [(a,b)] -> [b]
find k xs = [v | (k',v) <- xs, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [1..(length xs)])

-- 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct u v = sum [x*y | (x,y) <- zip u v]

-- 10
let2int :: Char -> Int
let2int c
  | ord c < 97 = ord c - ord 'A'
  | otherwise  = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isAlpha c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
