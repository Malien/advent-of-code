{-# LANGUAGE NamedFieldPuns #-}
import           Data.List.Split (splitOn)

main = readFile "in" >>= print . process

test = "\
\Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
\Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
\Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
\Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
\Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

process =
    sum
  . map (setPower . maxDice . parseGame)
  . lines

data GameSet = GameSet { red :: Int, green :: Int, blue :: Int } deriving Show
data Color = Red | Green | Blue deriving (Show, Eq, Ord)

emptySet = GameSet { blue = 0, red = 0, green = 0 }

parseGame = 
    map parseGameSet 
  . splitOn "; " 
  . (!! 1) -- drop "Game X: "
  . splitOn ": "

parseGameSet =
    foldl accumGameSet emptySet
  . map parseDice
  . splitOn ", "

accumGameSet set@(GameSet { red   }) (Red  , amount) = set { red   = red   + amount }
accumGameSet set@(GameSet { green }) (Green, amount) = set { green = green + amount }
accumGameSet set@(GameSet { blue  }) (Blue , amount) = set { blue  = blue  + amount }

parseDice str = (parseColor color, read amount)
  where [amount, color] = words str

parseColor "blue"  = Blue
parseColor "red"   = Red
parseColor "green" = Green

maxDice games = GameSet {
  red = maximum $ map red games,
  green = maximum $ map green games,
  blue = maximum $ map blue games
}

setPower (GameSet { red, green, blue }) = red * green * blue
