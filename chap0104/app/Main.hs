module Main where

main :: IO ()
main = do
    putStrLn $ convert6 123456

units, teens, tens :: [String]
units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
-- >>> convert1 0
-- "zero"
-- >>> convert1 1
-- "one"
-- >>> convert1 2
-- "two"
-- >>> convert1 20
-- "TODO"
convert1 :: Int -> String
convert1 n | n < 10 = units!!n
convert1 _ = "TODO"

digits2 :: Int -> (Int, Int)
digits2 n = (n `div` 10, n `mod` 10)

-- >>> convert2 10
-- "ten"
-- >>> convert2 11
-- "eleven"
-- >>> convert2 20
-- "twenty"
-- >>> convert2 21
-- "twenty-one"
-- >>> convert2 22
-- "twenty-two"
convert2 :: Int -> String
convert2 = combine2 .digits2

combine2 :: (Int, Int) -> String
combine2 (t, u)
    | t==0         = units!!u
    | t==1         = teens!!u
    | u==0         = tens!!(t-2)
    | otherwise    = tens!!(t-2) ++ "-" ++ units!!u

-- >>> convert3 300
-- >>> convert3 100
-- >>> convert3 101
-- >>> convert3 111
-- >>> convert3 120
-- >>> convert3 121
-- "three hundred"
-- "one hundred"
-- "one hundred and one"
-- "one hundred and eleven"
-- "one hundred and twenty"
-- "one hundred and twenty-one"
convert3 :: Int -> String
convert3 n
    | h==0      = convert2 t
    | t==0      = units!!h ++ " hundred"
    | otherwise = units!!h ++ " hundred and " ++ convert2 t
    where (h,t) = (n `div` 100, n `mod` 100)

-- >>> convert6 100300
-- >>> convert6 101100
-- >>> convert6 120101
-- >>> convert6 300111
-- >>> convert6 310120
-- >>> convert6 311121
-- "one hundred thousand three hundred"
-- "one hundred and one thousand one hundred"
-- "one hundred and twenty thousand one hundred and one"
-- "three hundred thousand one hundred and eleven"
-- "three hundred and ten thousand one hundred and twenty"
-- "three hundred and eleven thousand one hundred and twenty-one"
convert6 :: Int -> String
convert6 n
    | m==0      = convert3 h
    | h==0      = convert3 m ++ " thousand"
    | otherwise = convert3 m ++ " thousand " ++ link h ++ convert3 h
    where (m,h) = (n `div` 1000, n `mod` 1000)

link :: Int -> String
link h = if h < 100 then " and " else ""
