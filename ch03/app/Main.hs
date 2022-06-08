module Main where

import Data.List (sortBy)
import GHC.Classes (compare)

-- Ex01 and Ex02: Number of elements in a list
lenList :: [a] -> Integer
lenList []     = 0
lenList (_:xs) = 1 + lenList xs

-- Ex03: Mean value of elements in a list
meanList :: [Integer] -> Maybe Double
meanList [] = Nothing
meanList x  = Just $ fromIntegral (sumList x) / fromIntegral (lenList x)
    where 
        sumList :: [Integer] -> Integer
        sumList []     = 0
        sumList (x:xs) = x + sumList xs

-- Ex04: Turn a list into a palindrome
listToPalindrome :: [a] -> [a]
listToPalindrome x = x ++ reverse x

-- Ex05: Tell whether a list is a palindrome or not
isPalindrome :: Eq a => [a] -> Bool
isPalindrome []     = True
isPalindrome (x:[]) = True
isPalindrome x      = if head x == last x
                      then isPalindrome (tail (reverse (tail x)))
                      else False

-- Ex06: Sort a list of lists based on the length of each sublist
sortLists :: Ord a => [[a]] -> [[a]]
sortLists x = sortBy compare x

-- Ex07: Join list of lists together using a separator value
interperse :: a -> [[a]] -> [a]
interperse _ [] = []
interperse _ (x:[]) = x
interperse s (x:xs) = x ++ [s] ++ interperse s xs

-- Ex08: Height of binary tree
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

heightTree :: Tree a -> Integer
heightTree Empty                = 0
heightTree (Node _ Empty Empty) = 1
heightTree (Node _ t1 t2)       = if heightTree t1 >= heightTree t2
                                  then 1 + heightTree t1
                                  else 1 + heightTree t2

-- Ex09: Data type to define possible angle turns: right, left, stright
data Point = Point (Double, Double)
           deriving (Show)

data Direction = DRight
               | DLeft
               | DStraight
               deriving (Show)

-- Ex10: Calculate the turn made by three 2D points and returns a Direction
turn :: Point -> Point -> Point -> Direction
turn p1 p2 p3  = case compare (slope p1 p2) (slope p2 p3) of
               LT -> DLeft
               EQ -> DStraight
               GT -> DRight
            where
                slope :: Point -> Point -> Double
                slope  (Point (p11,p12)) (Point (p21,p22)) = (p22-p12) / (p21-p11)

-- Ex11: Compute lists of turns from list of 2D Points
toDirections :: [Point] -> [Direction]
toDirections []       = []
toDirections (x:[])   = []
toDirections (x:y:[]) = []
toDirections x        = [turnList (take 3 x)] ++ toDirections (tail x)
    where
        turnList :: [Point] -> Direction
        turnList x = turn (x !! 0) (x !! 1) (x !! 2)

-- Ex12: Graham scan algorithm
points :: [Point]
points = [Point (46, 62), Point (0, 40), Point (79, 61), Point (34, 57), Point (52, 10), Point (52, 27), Point (90, 15), Point (54, 99), Point (94, 15), Point (58, 8), Point (28, 53), Point (66, 35), Point (58, 22), Point (59, 7), Point (43, 36), Point (57, 51), Point (30, 57), Point (13, 8), Point (54, 88), Point (59, 32)]



main :: IO ()
main = putStrLn "Hello, Haskell!"
