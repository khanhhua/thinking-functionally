module MonadicPipeline where

main :: IO ()
main =
    safeDivide <$> readInt <*> readInt >>= showMaybe


showMaybe :: (Show a) => Maybe a -> IO ()
showMaybe a =
    case a of
        Nothing -> putStrLn "It is nothing"
        Just value -> putStrLn (show value)


divide :: Integral a => Integer -> Integer -> a
divide a b =
    (fromInteger a) `div` (fromInteger b)


safeDivide :: Integral a => Maybe Integer -> Maybe Integer -> Maybe a
safeDivide a b =
    case b of
        Just 0 -> Nothing
        _ -> pure divide <*> a <*> b


parseInteger :: String -> Maybe Integer
parseInteger s =
    case trim(s) of
        "" -> Nothing
        _ -> Just (read s :: Integer)
    where
        trim = unwords . words


readInt :: IO (Maybe Integer)
readInt = parseInteger <$> getLine
