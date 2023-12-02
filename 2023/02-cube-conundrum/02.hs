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
  . map gameId 
  . filter (possibleGame permitted) 
  . map parseGame 
  . lines

data GameSet = GameSet { red :: Int, green :: Int, blue :: Int } deriving Show
data Game = Game { gameId :: Int, sets :: [GameSet] } deriving Show
data Color = Red | Green | Blue deriving (Show, Eq, Ord)

emptySet = GameSet { red = 0, green = 0, blue = 0 }

parseGame line = Game { gameId = read idStr, sets = sets }
  where [gamePart, setsPart] = splitOn ": " line
        ["Game", idStr] = words gamePart
        sets = map parseGameSet $ splitOn "; " setsPart

parseGameSet =
    foldl accumGameSet emptySet
  . map parseDice
  . splitOn ", "

accumGameSet set@(GameSet { red   }) (Red  , amount) = set { red   = red   + amount }
accumGameSet set@(GameSet { green }) (Green, amount) = set { green = green + amount }
accumGameSet set@(GameSet { blue  }) (Blue , amount) = set { blue  = blue  + amount }

parseDice str = (parseColor colorStr, read amountStr)
  where [amountStr, colorStr] = words str

parseColor "red"   = Red
parseColor "green" = Green
parseColor "blue"  = Blue

permitted = GameSet { red = 12, green = 13, blue = 14 }

possibleSet allowed game =
  red   game <= red   allowed &&
  blue  game <= blue  allowed &&
  green game <= green allowed

possibleGame allowed game = all (possibleSet allowed) (sets game)
