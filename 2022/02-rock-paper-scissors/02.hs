main = process <$> readFile "in" >>= print

data Play = Rock | Paper | Scissors deriving Eq

playScore Rock     = 1
playScore Paper    = 2
playScore Scissors = 3

parsePlay "A" = Rock
parsePlay "B" = Paper
parsePlay "C" = Scissors
parsePlay "X" = Rock
parsePlay "Y" = Paper
parsePlay "Z" = Scissors

data Outcome = Win | Lose | Draw

outcomeScore Win  = 6
outcomeScore Draw = 3
outcomeScore Lose = 0

outcome Rock Paper     = Win
outcome Paper Scissors = Win
outcome Scissors Rock  = Win
outcome x y            | x == y = Draw
outcome _ _            = Lose

parseLine line = (parsePlay his, parsePlay mine)
  where [his, mine] = words line

score his mine = outcomeScore (outcome his mine) + playScore mine

process = sum . map (uncurry score . parseLine) . lines
