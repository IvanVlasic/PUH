import Data.Char
import Data.List
import System.Environment

-- =================================================

-- Problem 1.

-- a)
toTitleCase :: String -> String
toTitleCase s = unwords [toUpper c : w | (c:w) <- words s] 

-- b)

toTitleCase' :: String -> [String] -> String
toTitleCase' s w = unwords $ cap x : toTitleCase'' xs w 
  where (x:xs) = words s

toTitleCase'' :: [String] -> [String] -> [String]
toTitleCase'' []     _ = []
toTitleCase'' (x:xs) w
  | x `elem` w = x : toTitleCase'' xs w
  | otherwise  = cap x : toTitleCase'' xs w

-- capitalize first letter of the word
cap :: [Char] -> [Char]
cap s = (toUpper . head $ s) : tail s

-- =================================================

-- Problem 2.

trimN :: [a] -> Int -> [a]
trimN xs n
  | 2 * n > length xs = xs
  | otherwise         = drop n $ take (length xs - n) $ xs

-- =================================================

-- Problem 3.

main :: IO ()
main = do
  args <- getArgs
  content <- readFile . head $ args
  putStrLn [toUpper c | c <- content]

-- =================================================

-- Problem 4.

onlyDivisible :: String -> Integer -> String
onlyDivisible s n
  | n <= 0    = error "n must be positive!"
  | otherwise = [c | (m, c) <- zip [0..] s, m `mod` n == 0]

-- =================================================

-- Problem 5.

areCollinear :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
areCollinear (a,b) (m,n) (x,y)
  | (n - b) * (x - m) == (y - n) * (m - a) = 0
  | otherwise = 1

triangleCounter :: [(Int, Int)] -> Int
triangleCounter xs = sum [ areCollinear x y z | (i, x) <- indexes xs, (j, y) <- indexes xs, (k, z) <- indexes xs,
  i < j, j < k]

indexes :: [b] -> [(Integer, b)]
indexes = zip [1..]

-- =================================================

-- Problem 6.

reverseWords :: String -> String
reverseWords = unwords . reverse . words

-- =================================================

-- Problem 7.

intersects' :: Eq t => [t] -> [t] -> [t]
intersects' xs ys
  | null xs || null ys = []
  | otherwise = [x | x <- xs, x `elem` ys]

difference :: Eq t => [t] -> [t] -> [t]
difference xs ys
  | null xs || null ys = []
  | otherwise = [x | x <- xs, x `notElem` ys]

-- =================================================

-- Problem 8.

-- a)
isWellFormed :: [[a]] -> Bool
isWellFormed [[]] = False
isWellFormed xs = and [length x == (length $ head xs) | x <- xs]

-- b)
size :: [[a]] -> (Int, Int)
size xs
  | not $ isWellFormed xs = error "Matrix is malformed"
  | otherwise = (length xs, length $ head xs) 

-- c)
getElement :: [[Int]] -> Int -> Int -> Int
getElement xs m n
  | not $ isWellFormed xs = error "Matrix is malformed"
  | m >= m' || n >= n' = error "Index out of bounds"
  | m < 0 || n < 0 = error "Negative index"
  | otherwise = xs !! m !! n
  where (m', n') = size xs

-- d)
getRow :: [[Int]] -> Int -> [Int]
getRow xs m
  | not $ isWellFormed xs = error "Matrix is malformed"
  | m >= m' || m < 0 = error "Index out of bounds"
  | otherwise = xs !! m
  where (m', _) = size xs

-- e)
getCol :: [[Int]] -> Int -> [Int]
getCol xs n
  | not $ isWellFormed xs = error "Matrix is malformed"
  | n >= n'  || n < 0 = error "Index out of bounds"
  | otherwise = [x !! n | x <- xs]
  where (_, n') = size xs

-- f)
addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices xs ys
  | (not $ isWellFormed xs) || (not $ isWellFormed ys) = error "Matrix is malformed"
  | m1 /= m2 || n1 /= n2 = error "Matrices are not of equal size"
  | otherwise = [[getElement xs i j + getElement ys i j | j <- [0..n1 - 1]] | i <- [0..m1 - 1]]
  where (m1, n1) = size xs
        (m2, n2) = size ys

-- g)
transpose' :: [[Int]] -> [[Int]]
transpose' xs
  | not $ isWellFormed xs = error "Matrix is malformed"
  | otherwise = [getCol xs i | i <- [0..(snd $ size xs) - 1]]

-- h)
multMatrices :: [[Int]] -> [[Int]] -> [[Int]]
multMatrices xs ys
  | (not $ isWellFormed xs) || (not $ isWellFormed ys) = error "Matrix is malformed"
  | n1 /= m2 = error "Incompatible matrix dimensions"
  | otherwise = [[multLists x (getCol ys n) | n <- [0..n2 - 1]] | x <- xs]
  where (m1, n1) = size xs
        (m2, n2) = size ys

multLists :: [Int] -> [Int] -> Int
multLists xs ys = sum $ zipWith (*) xs ys
