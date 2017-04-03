import Data.Vector hiding (take, sum)
import qualified Data.Vector as Vector
import System.Random
import System.IO.Unsafe
import Control.Concurrent (threadDelay)
import System.Environment

type World = Vector (Vector Bool)
miniWorld = fromList $ Prelude.map fromList
  [[True, True, False],
  [False, True, True],
  [False, True, False]]

-- a)
randBools :: Int -> IO [Bool]
randBools n = do
  g <- newStdGen
  let bools = randoms g :: [Bool]
  return $ take n bools

genesis :: Int -> Int -> IO World
genesis width height
  | width < 1 || height < 1 = error "Invalid width or height"
  | otherwise = return $ fromList $ Prelude.map fromList [unsafePerformIO $ randBools width | _ <- [1..height]]

-- b)
showWorld :: World -> String
showWorld world = unlines $ Prelude.map (Prelude.map showWorld' . toList) $ toList world
  where
    showWorld' True = 'x'
    showWorld' False = ' '

-- c)
size :: World -> (Int, Int)
size world = (Vector.length world,Vector.length $ world!0)

positions = [(i,j) | i <- [-1,0,1], j <- [-1,0,1], i /= 0 || j /=0]
neighbours :: World -> Int -> Int -> Int
neighbours world x y = sum [1 | (i,j) <- positions, check (x+i) (y+j)]
  where
    check i j
      | i < 0 || j < 0 || i >= m || j >= n = False
      | otherwise = world!i!j
      where
        (m,n) = size world

-- d)
nextState :: World -> Int -> Int -> Bool
nextState world x y
  | alive && n < 2 = False
  | alive && n < 4 = True
  | alive = False
  | n == 3 = True
  | otherwise = False
   where
     n = neighbours world x y
     alive = world!x!y

-- e)
gameStep :: World -> World
gameStep world = fromList $ Prelude.map fromList $ newWorld
  where
    newWorld = [[nextState world i j | j <- [0..w-1]] | i <- [0..h-1]]
    (h,w) = size world

-- f)
play :: World -> IO ()
play world = do
  putStrLn $ Prelude.replicate 10 '='
  putStr $ showWorld world
  threadDelay $ 10^6
  play $ gameStep world

-- g)
main :: IO ()
main = do
  args <- getArgs
  let (h,w) = (read (args!!0) :: Int, read (args!!1) :: Int)
  world <- genesis h w
  play $ world

test :: IO ()
test = do
  world <- genesis 10 10
  play $ world
