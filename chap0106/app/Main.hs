module Main where

-- >>> map double [1,2,3,4]
double :: Integer -> Integer
double x = 2 * x

main :: IO ()
main = do
    putStrLn "Hello world!"


song :: Int -> String
song n =
    if n==0 then ""
    else song (n-1) ++ "\n" ++ verse n

verse :: Int -> String
verse n =
    line1 n ++ line2 ++ line3 n ++ line2
  where
    units = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    line1 n
        | n == 1    = units!!(n -1) ++ " man went to mow\n"
        | otherwise = units!!(n -1) ++ " men went to mow\n"
    line2 = "Went to mow a meadow\n"
    line3 n
        | n == 1    = units!!(n - 1) ++ " man and his dog\n"
        | otherwise = units!!(n - 1) ++ " men, " ++ line3 (n - 1)
