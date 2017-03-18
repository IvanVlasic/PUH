import Data.Char
import Data.List

-- EXERCISE 1 ======================================

-- 1.1
-- Define a recursive function to compute the product of a list of elements.

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

-- 1.2
-- Define a recursive function 'headsOf' that takes a list of lists and returns
-- a list of their heads.

headsOf :: [[a]] -> [a]
headsOf [] = []
headsOf ([]:xs) = headsOf xs
headsOf ((x:_):xs) = x : headsOf xs

-- EXERCISE 2 ======================================

-- 2.1
-- Define a recursive function 'modMult n m xs' that multiplies each element of
-- a lit x with 'n' modulo 'm'.

modMult :: Integral a => a -> a -> [a] -> [a]
modMult n m xs = modMult' (n `mod` m) xs
  where
    modMult' k [] = []
    modMult' k (x:xs) = k*x : modMult' k xs

-- 2.2
-- Define a function 'addPredecessor' that adds to each element of a list the
-- value of the preceding element. The first element gets no value added.
-- addPredecessor :: Num a => [a] -> [a]
-- addPredecessor [3,2,1] => [3,5,3]

addPredecessor :: Num a => [a] -> [a]
addPredecessor [] = []
addPredecessor xs = addPredecessor' 0 xs
  where
    addPredecessor' _ [] = []
    addPredecessor' n (x:xs) = n + x : addPredecessor' x xs

-- EXERCISE 3 ======================================

-- 3.1
-- Define 'equalTriplets' that filters from a list of triplets (x,y,z) all
-- triplets for which x==y==z.
-- equalTriplets [(1,2,3),(2,2,2),(4,5,6)] => [(2,2,2)]

equalTriplets :: Eq a => [(a,a,a)] -> [(a,a,a)]
equalTriplets [] = []
equalTriplets ((x,y,z):xs)
  | x == y && x == z = (x,y,z) : equalTriplets xs
  | otherwise = equalTriplets xs

-- 3.2
-- Define your own version of the replicate function:

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n-1) x

-- EXERCISE 4 ======================================

-- 4.1
-- Define your own recursive version of the drop function:
-- drop' :: Int -> [a] -> [a].
-- Define drop'' (a wrapper function) so that for n < 0 the function drops
-- the elements from the end of the list. You can use 'reverse'.

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n a@(x:xs)
  | n <= 0 = a
  | otherwise = drop' (n-1) xs

drop'' :: Int -> [a] -> [a]
drop'' n xs
  | n < 0 = reverse $ drop' (-n) $ reverse xs
  | otherwise = drop' n xs

-- 4.2
-- Define a recursive function 'takeFromTo n1 n2 xs'.

takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo _ _ [] = []
takeFromTo n m (x:xs)
  | n == 0 && m >= 0 = x : takeFromTo n (m-1) xs
  | n == 0 = []
  | otherwise = takeFromTo (n-1) (m-1) xs

-- EXERCISE 5 ======================================

-- 5.1
-- Define a recursive function 'eachThird' that retains every third element
-- in a list.

eachThird :: [a] -> [a]
eachThird (x:y:z:xs) = z : eachThird xs
eachThird _ = []

-- EXERCISE 6 ======================================

-- 6.1
-- Write an accumulator-style recursive definition of
-- length' :: [a] -> Int

length' :: [a] -> Int
length' xs = len xs 0
  where
    len [] n = n
    len (_:xs) n = len xs (n+1)

-- 6.2
-- Write an accumulator-style recursive definition of
-- maxUnzip :: [(Int,Int)] -> (Int,Int)
-- that returns the maximum element at the first position and the maximum
-- element at the second position in a pair, i.e., it's equivalent to:
-- maxUnzip zs = (maximum xs, maximum ys)
-- where (xs,ys) = unzip zs
-- If the list is empty, return an "empty list" error.
-- Now write a standard recursive definition (without an
-- accumulator).

maxUnzip :: [(Int,Int)] -> (Int,Int)
maxUnzip [] = error "empty list"
maxUnzip [(x,y)] = (x,y)
maxUnzip ((x,y):xs) = (max x x', max y y')
  where
    (x',y') = maxUnzip xs

maxUnzip2 :: [(Int,Int)] -> (Int,Int)
maxUnzip2 [] = error "empty list"
maxUnzip2 ((x,y):xs) = maxUnzip' xs x y
  where
    maxUnzip' [] x y = (x,y)
    maxUnzip' ((x',y'):xs) x y = maxUnzip' xs (max x x') (max y y')
