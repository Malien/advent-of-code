import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Data.List.Split

main = readFile "input.txt" >>= print . solution . lines

solution = sum . map solveClock . map parseClock

solveClock (digits, input) = intoDecimalNumber $ map (transformSegments $ segmentMapping digits) input

parseClock str = (words nums, words input)
    where [nums, input] = splitOn " | " str

intoDecimalNumber = foldl (\acc x -> acc * 10 + x) 0

transformSegments mapping digits = fromJust $ findIndex (== activeSegments) digitSegments
    where activeSegments = Set.fromList $ map (mapDigit mapping) digits

segmentMapping digits = fromJust $ find (validMapping digits) $ permutations "abcdefg"

digitSegments = map Set.fromList [
    [0, 1, 2, 4, 5, 6],
    [2, 5],
    [0, 2, 3, 4, 6],
    [0, 2, 3, 5, 6],
    [1, 2, 3, 5],
    [0, 1, 3, 5, 6],
    [0, 1, 3, 4, 5, 6],
    [0, 2, 5],
    [0, 1, 2, 3, 4, 5, 6],
    [0, 1, 2, 3, 5, 6]
    ]

validMapping digits mapping = all (`elem` digitSegments) $ map (Set.fromList . map (mapDigit mapping)) digits

mapDigit mapping digit = fromJust $ findIndex (==digit) mapping

