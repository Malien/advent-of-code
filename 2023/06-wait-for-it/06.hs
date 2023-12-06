main = readFile "in" >>= print . process

test = "\
\Time:      7  15   30\n\
\Distance:  9  40  200"

process = product . map findTimes . parseRaces

parseRaces str = zip time dist
    where [timeLine, distLine] = lines str
          time = map read $ words $ drop (length "Time: ") timeLine
          dist = map read $ words $ drop (length "Distance: ") distLine

simulation holdingT raceT = holdingT * raceT

divideTime time = [(holdingT, time - holdingT) | holdingT <- [0..time]]

findTimes :: (Int, Int) -> Int
findTimes (time, dist) = length $ filter (>dist) $ map (uncurry simulation) $ divideTime time

