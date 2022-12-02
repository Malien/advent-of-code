import           Data.List.Split

main = process <$> readFile "in" >>= print

data Play = Rock | Paper | Scissors deriving Eq

playScore Rock     = 1
playScore Paper    = 2
playScore Scissors = 3

parsePlay "A" = Rock
parsePlay "B" = Paper
parsePlay "C" = Scissors

data Outcome = Win | Lose | Draw

parseOutcome "X" = Lose
parseOutcome "Y" = Draw
parseOutcome "Z" = Win

stateScore Win  = 6
stateScore Draw = 3
stateScore Lose = 0

decide Draw x        = x
decide Win Rock      = Paper
decide Win Paper     = Scissors
decide Win Scissors  = Rock
decide Lose Paper    = Rock
decide Lose Scissors = Paper
decide Lose Rock     = Scissors

parse inp = (parsePlay enemyPlay, parseOutcome desiredOutcome)
  where [enemyPlay, desiredOutcome] = splitOn " " inp

score enemyPlay outcome = stateScore outcome + playScore (decide outcome enemyPlay)

process = sum . map (uncurry score . parse) . lines
