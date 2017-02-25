module CSVUtils (
   Separator
  ,Document
  ,Field
  ,Entry
  ,CSV
  ,parseCSV
  ,showCSV
  ,colFields
  ,readCSV
  ,writeCSV
) where

import Data.List
import Data.List.Split

-- =================================================

-- Problem 8.

type Separator = String
type Document  = String
type CSV       = [Entry]
type Entry     = [Field]
type Field     = String

doc       = "John;Doe;15\nTom;Sawyer;12\nAnnie;Blake;20"
brokenDoc = "One;Two\nThree;Four;Five"

-- a)
parseCSV :: Separator -> Document -> CSV
parseCSV sep doc
  | not $ containsSep sep doc = error $ "The character '" ++ sep ++ "' does not occur in text"
  | isMalformedCSV csv = error "The CSV file is not well-formed"
  | otherwise = csv
  where csv = parseCSV' sep doc

parseCSV' :: Separator -> Document -> CSV
parseCSV' sep doc = [splitOn sep x | x <- lines doc]

isMalformedCSV :: CSV -> Bool
isMalformedCSV csv = not $ all ((== (length $ head csv)) . length) csv

containsSep :: Separator -> Document -> Bool
containsSep sep doc = or [isInfixOf sep x | x <- lines doc]

-- b)
showCSV :: Separator -> CSV -> Document
showCSV sep csv
  | isMalformedCSV csv = error "The CSV file is not well-formed"
  | otherwise          = unlines $ map (intercalate sep) csv

-- c)
colFields :: Int -> CSV -> [Field]
colFields n csv
  | (length $ head csv) <= n = error $ "There is no column " ++ show n ++ " in the CSV document"
  | otherwise = map (!! n) csv

-- d)
readCSV :: Separator -> FilePath -> IO CSV
readCSV sep path = do
  f <- readFile path
  return $ parseCSV sep f

-- e)
writeCSV :: Separator -> FilePath -> CSV -> IO ()
writeCSV sep path csv = do
  let doc = showCSV sep csv
  writeFile path doc
