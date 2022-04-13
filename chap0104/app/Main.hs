module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- >>> convert 0
-- "Zero"

-- >>> convert 1
-- "???"
convert :: Int -> String
convert 0 = "Zero"
convert _ = "TODO"
