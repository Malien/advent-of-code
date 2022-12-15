{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}
import           Data.List       (sortOn)
import           Data.Maybe      (catMaybes)
import           Text.Regex.PCRE (AllTextMatches (getAllTextMatches), (=~))

main = readFile "in" >>= print . process

process inp = x * 4_000_000 + y
  where readings = map parseReading $ lines inp
        (y, x) = head $ concatMap (findHoles readings) [0 .. 4_000_000]

findHoles readings y = map (y,) $ holes $ clampRanges (0, 4_000_000) $ overlappings y readings

holes ranges = zipWith (\(_, prevMin) _ -> prevMin) ranges (tail ranges)

overlappings y = combineAllRanges . sortOn fst . catMaybes . map (xrange y)

clampRanges _ [] = []
clampRanges c@(clampMin, clampMax) ((minRange, maxRange):rest)
  | clampMin > maxRange = clampRanges c rest
  | clampMin > minRange = (clampMin, min maxRange clampMax) : clampRanges c rest
  | clampMax < minRange = []
  | clampMax < maxRange = [(minRange, clampMax)]
  | otherwise           = (minRange, maxRange) : clampRanges c rest

manhattanDistance (ay, ax) (by, bx) = abs (ax - bx) + abs (ay - by)

data RangeCombination = Combined (Int, Int) | Disjoint (Int,Int) (Int, Int) deriving Show

combineAllRanges [] = []
combineAllRanges [x] = [x]
combineAllRanges (prev:next:rest) = case combineRange prev next of
  Combined range     -> combineAllRanges (range:rest)
  Disjoint prev next -> prev : combineAllRanges (next:rest)

combineRange (prevMinX, prevMaxX) (nextMinX, nextMaxX)
  | nextMinX <= prevMaxX = Combined (prevMinX, max prevMaxX nextMaxX)
combineRange prev next = Disjoint prev next

xrange y (Reading { sensor = (sensorY,sensorX), distance })
  | distance < distanceTilY = Nothing
  | otherwise               = Just (sensorX - xDiff, sensorX + xDiff + 1)
  where distanceTilY = abs (sensorY - y)
        xDiff = abs (distance - distanceTilY)

data Reading = Reading {
  sensor   :: (Int, Int),
  beacon   :: (Int, Int),
  distance :: Int
  } deriving Show

parseReading inp = Reading { sensor, beacon, distance = manhattanDistance sensor beacon }
  where [sensorX, sensorY, beaconX, beaconY] = map read $ getAllTextMatches (inp =~ "-?\\d+")
        sensor = (sensorY, sensorX)
        beacon = (beaconY, beaconX)

test =
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\n\
  \Sensor at x=9, y=16: closest beacon is at x=10, y=16\n\
  \Sensor at x=13, y=2: closest beacon is at x=15, y=3\n\
  \Sensor at x=12, y=14: closest beacon is at x=10, y=16\n\
  \Sensor at x=10, y=20: closest beacon is at x=10, y=16\n\
  \Sensor at x=14, y=17: closest beacon is at x=10, y=16\n\
  \Sensor at x=8, y=7: closest beacon is at x=2, y=10\n\
  \Sensor at x=2, y=0: closest beacon is at x=2, y=10\n\
  \Sensor at x=0, y=11: closest beacon is at x=2, y=10\n\
  \Sensor at x=20, y=14: closest beacon is at x=25, y=17\n\
  \Sensor at x=17, y=20: closest beacon is at x=21, y=22\n\
  \Sensor at x=16, y=7: closest beacon is at x=15, y=3\n\
  \Sensor at x=14, y=3: closest beacon is at x=15, y=3\n\
  \Sensor at x=20, y=1: closest beacon is at x=15, y=3"
