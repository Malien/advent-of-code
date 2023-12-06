main = readFile "in" >>= print . process

test = "\
\Time:      7  15   30\n\
\Distance:  9  40  200"

process = findTimes . parseRace

parseRace str = (time, dist)
    where [timeLine, distLine] = lines str
          time = read $ concat $ words $ drop (length "Time: ") timeLine
          dist = read $ concat $ words $ drop (length "Distance: ") distLine

simulation holdingT raceT = holdingT * raceT

divideTime time = [(holdingT, time - holdingT) | holdingT <- [0..time]]

findTimes :: (Int, Int) -> Int
findTimes (time, dist) = length $ filter (>dist) $ map (uncurry simulation) $ divideTime time

