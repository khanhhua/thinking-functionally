module Main where

import Options.Applicative
import Text.Printf

data Calculator = Calculator
    { precision :: String
    , operand :: String
    }

main :: IO ()
main = do
    myCalculator <- execParser opts 
    let
        result = calc myCalculator
    putStrLn result
  where
    opts :: ParserInfo Calculator
    opts =
        info (helper <*> versionOption <*> programOptions)
            ( fullDesc 
            <> progDesc "An overkill Calculator"
            <> header "cabal run calc - should do it"
            )
    versionOption :: Parser (a -> a)
    versionOption = infoOption "1.0" (long "version" <> help "Show version")

    programOptions :: Parser Calculator
    programOptions =
        Calculator <$>
            strOption (long "precision" <> metavar "VALUE" <> help "Set the precision") <*>
            strOption (long "operand" <> metavar "VALUE" <> help "Set the value to work on")

calc :: Calculator -> String
calc calculator =
    let
        operandValue = read (operand calculator) :: Float
    in printf ("%." <> (precision calculator) <> "f") operandValue
