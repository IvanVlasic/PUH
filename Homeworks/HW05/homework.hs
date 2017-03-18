import Data.Char
import Data.List

-- =================================================

-- Problem 1.

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' c (x:xs) = x : prependToAll c xs

prependToAll :: a -> [a] -> [a]
prependToAll _ [] = []
prependToAll c (x:xs) = c : x : prependToAll c xs

intercalate' :: [a] -> [[a]] -> [a]
intercalate' c xs = concat $ intersperse' c xs

-- =================================================

-- Problem 2.

type Probability = Double
type DiscreteRandVar = [(Int, Probability)]

x :: DiscreteRandVar
x = [(1, 0.2), (2, 0.4), (3, 0.1), (4, 0.2), (5, 0.05), (6, 0.05)]

-- a)
mean :: DiscreteRandVar -> Double
mean [] = 0.0
mean ((x,p):xs) = (fromIntegral x)*p + mean xs

mean' :: DiscreteRandVar -> Double
mean' xs = mean'' xs 0.0
  where mean'' [] n = n
        mean'' ((x,p):xs) n = mean'' xs ((fromIntegral x)*p + n)

-- b)
variance :: DiscreteRandVar -> Double
variance xs = step xs
  where u = mean xs
        step [] = 0.0
        step ((x,p):xs) = (u - fromIntegral x)^2 * p + step xs

variance' :: DiscreteRandVar -> Double
variance' xs = variance'' xs 0.0
  where u = mean' xs
        variance'' [] n = n
        variance'' ((x,p):xs) n = variance'' xs (( - fromIntegral x)^2 * p + n)

-- c)
probabilityFilter :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter _ [] = []
probabilityFilter p ((x,p'):xs)
  | p' >= p = x : probabilityFilter p xs
  | otherwise = probabilityFilter p xs

probabilityFilter' :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter' p xs = reverse $ probabilityFilter'' p xs []
  where probabilityFilter'' _ [] vs = vs
        probabilityFilter'' p ((x,p'):xs) vs
          | p' >= p = probabilityFilter'' p xs (x:vs)
          | otherwise = probabilityFilter'' p xs vs

-- =================================================

-- Problem 3.

-- a)
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs
  | n <= 0 = []
  | otherwise = take n xs : (chunk n $ drop n xs)

-- b)
chunkBy :: [Int] -> [a] -> [[a]]
chunkBy _ [] = []
chunkBy [] xs = []
chunkBy (n:ns) xs
  | n <= 0 = chunkBy ns xs
  | otherwise = take n xs : (chunkBy ns $ drop n xs)

-- c)
getSizes :: Int -> Int -> Int -> [Int]
getSizes 1 _ len = [len]
getSizes n size len = [size] ++ getSizes (n-1) size (len-size)

chunkInto :: Int -> [a] -> [[a]]
chunkInto n xs
  | n <= 0 = []
  | otherwise = chunkBy sizes xs
  where size = max 1 $ div (length xs) n
        sizes = getSizes n size $ length xs

-- =================================================

-- Problem 4.

ops = ['+', '-', '*', '/', '^']

operators = [
  ('+', (+))
 ,('-', (-))
 ,('*', (*))
 ,('/', (div))
 ,('^', (^))
 ]

rpnCalc :: String -> Int
rpnCalc [] = 0
rpnCalc xs = rpnCalc' xs []
  where rpnCalc' [] [s] = s
        rpnCalc' (x:xs) s
          | isDigit x = rpnCalc' xs ((digitToInt x):s)
          | length s < 2 = error "Invalid RPN expression"
          | otherwise = rpnCalc' xs (doOp (getOp x) s)
        doOp op (x:y:xs) = (y `op` x):xs
        getOp x = snd $ head $ filter ((==x) . fst) operators

-- =================================================

-- Problem 5.

-- a)
gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

-- b)
gcdAll :: [Int] -> Int
gcdAll [] = error "Cannot compute gcd of an empty list"
gcdAll xs = foldr1 gcd' xs

-- c)
extendedGcd :: Int -> Int -> (Int, Int, Int)
extendedGcd 0 b = (0, 1, b)
extendedGcd a b = (x - (b `div` a) * y, y, g)
  where
    (y, x, g) = extendedGcd (b `mod` a) a

-- =================================================

-- Problem 6.

type AdjecencyList = [Int]
type Graph = [AdjecencyList]

isBipartite :: Graph -> Bool
isBipartite xs = and [ and [ null $ intersect x (xs!! (y - 1)) | y <- x] | x <- xs]

-- =================================================

-- Problem 7.

permutations' :: [a] -> [[a]]
permutations' [] = []
permutations' [x] = [[x]]
permutations' xs = concat [map (x:) $ permutations' ys | (x,ys) <- combs xs]
  where
    combs xs = [(xs !! i, map fst $ filter ((/= i) . snd) $ zip xs l) | i <- l]
    l     = [0..length xs - 1]

permutations'' [] = [[]]
permutations'' xs = [y:ys | y <- xs, ys <- permutations'' $ delete' y xs]

delete' _ [] = []
delete' y (x:xs)
  | x == y = xs
  | otherwise = x : delete' y xs

-- =================================================

-- Problem 8.

frogJumps :: Int -> Integer
frogJumps n = 3^n - 1
