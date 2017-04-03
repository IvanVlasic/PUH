import Data.Char
import Data.List hiding (partition)
import qualified Data.Set as Set
import qualified Data.Vector as V
import System.Random hiding (split)
import System.IO.Unsafe

-- =================================================

-- Problem 1.

partition :: [a -> Bool] -> [a] -> [[a]]
partition fs ys = [filter f ys | f <- fs]

-- =================================================

-- Problem 2.

cycleMap :: [a -> b] -> [a] -> [b]
cycleMap [] _ = []
cycleMap fs xs = zipWith ($) (cycle fs) xs

-- =================================================

-- Problem 3.

-- a)
reduce :: (a -> b -> a) -> a -> [b] -> a
reduce _ s [] = s
reduce f s (x:xs) = reduce f (f s x) xs

-- b)
reduce1 :: (a -> a -> a) -> [a] -> a
reduce1 _ [] = error "reduce1 got an empty list"
reduce1 f (x:xs) = reduce f x xs

-- c)
scan :: (a -> b -> a) -> a -> [b] -> [a]
scan _ s [] = [s]
scan f s (x:xs) = s : scan f (f s x) xs

-- =================================================

-- Problem 4.

-- a)
rreduce :: (a -> b -> b) -> b -> [a] -> b
rreduce f s xs = reduce (flip f) s $ reverse xs

-- b)
rreduce1 :: (a -> a -> a) -> [a] -> a
rreduce1 _ [] = error "rreduce1 got an empty list"
rreduce1 f xs = rreduce f (last xs) (init xs)

-- c)
rscan :: (a -> b -> b) -> b -> [a] -> [b]
rscan _ s [] = [s]
rscan f s (x:xs) = f x z : rscan f s xs
  where
    z = rreduce f s xs

-- =================================================

-- Problem 5.

-- a)
type Tolerance = Double

newton :: Tolerance -> Double -> Double
newton eps x
  | x < 0 = error "can't get sqrt of negative number"
  | otherwise = calc eps x 2
  where
    calc eps x y
      | eps >= abs (guess - y) = guess
      | otherwise = calc eps x guess
      where
        guess = (y + x/y) / 2

-- b)
deriv :: (Double -> Double) -> Double -> Double
deriv f x = (f (x + dx) - f x) / dx
  where
    dx = 0.00001

-- =================================================

-- Problem 6.

sumOfSquares :: Int -> Int
sumOfSquares = sum . map ((^2) . digitToInt) . show

isHappy :: Int -> Bool
isHappy num = isHappy' soas (Set.fromList [soas])
  where
    soas = sumOfSquares num
    isHappy' 1 _ = True
    isHappy' num set
      | Set.member soas' set = False
      | otherwise = isHappy' soas' (Set.insert soas' set)
      where
        soas' = sumOfSquares num

-- =================================================

-- Problem 7.

-- a)
split :: [a] -> ([a], [a])
split [] = ([], [])
split xs = split' n xs
  where 
    n = (length xs + 1) `div` 2
    split' 1 (x:xs) = ([x], xs)
    split' n (x:xs) = (x:xs', ys)
      where
        (xs', ys) = split' (n-1) xs

-- b)
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort x) (mergesort y)
  where
    (x,y) = split xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y = x : merge xs (y:ys)
  | x > y = y : merge (x:xs) ys
  | otherwise = x : y : merge xs ys

-- c)
merge' :: Ord a => [a] -> [a] -> Int -> ([a], Int)
merge' xs [] n = (xs,n)
merge' [] ys n = (ys,n)
merge' xss@(x:xs) (y:ys) n
  | x < y = (x:a,n1)
  | x > y = (y:b, n2)
  | otherwise = (x:y:c, n3)
  where
   (a, n1) = merge' xs (y:ys) n
   (b, n2) = merge' (x:xs) ys (n+length xss)
   (c, n3) = merge' xs ys n

mergesort'' [] n = ([],n)
mergesort'' [x] n = ([x],n)
mergesort'' xs n = (c,n1+n2+n3)
  where 
    (x,y) = split xs
    (a,n1) = mergesort'' x 0
    (b,n2) = mergesort'' y 0
    (c,n3) = merge' a b n

countInversions :: Ord a => [a] -> Int
countInversions [] = 0
countInversions [x] = 0
countInversions xs = n
  where
   (_,n) = mergesort'' xs 0

-- d)
mergesort' :: Ord a => V.Vector a -> V.Vector a
mergesort' xs
  | l < 2 = xs
  | otherwise = mergeVector (mergesort' x) (mergesort' y)
  where
    l = V.length xs
    (x,y) = V.splitAt (div (l+1) 2) xs

mergeVector :: Ord a => V.Vector a -> V.Vector a -> V.Vector a
mergeVector xs ys
  | V.null xs = ys
  | V.null ys = xs
  | x < y = V.cons x $ mergeVector xs' ys
  | x > y = V.cons y $ mergeVector xs ys'
  | otherwise = V.cons x $ V.cons y $ mergeVector xs' ys'
  where
    (x,xs') = (V.head xs, V.tail xs)
    (y,ys') = (V.head ys, V.tail ys)

-- =================================================

-- Problem 8.

type Package = Int
type Dependancy = (Package, Package)

createList :: Int -> [Dependancy] -> [(Package, [Package])]
createList n ds = [(i, x) | i <- [1..n], let x = map snd . filter((==i) . fst) $ ds]

checkIfValid :: Int -> [Dependancy] -> Bool
checkIfValid n = null . filter check
  where
    check (x,y) = check' x && check' y
    check' k = k < 1 || k > n

removeDepend :: [(Package, [Package])] -> Package -> [(Package, [Package])]
removeDepend ds p = filter (\(i, x) -> i /= p) ds'
  where
    ds' = [(i, delete p x) | (i,x) <- ds]

pacMan :: Int -> [Dependancy] -> [Package]
pacMan n ds
  | not $ checkIfValid n ds = error "Impossible to resolve"
  | otherwise = solve (createList n ds)
  where
    solve :: [(Package, [Package])] -> [Package]
    solve [] = []
    solve ds
      | null $ packages = error "Impossible to resolve"
      | otherwise = (fst . head $ packages) : (solve $ removeDepend ds (fst . head $ packages))
      where
        packages = filter (\(i,x) -> null x) ds

-- =================================================

-- Problem 9.

-- GameOfLife.hs
