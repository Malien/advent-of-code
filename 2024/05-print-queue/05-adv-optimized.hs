import           Data.Array      (Array, accum, listArray, (!))
import           Data.List       (sortBy)
import           Data.List.Split (splitOn)

main = readFile "in" >>= print . process

test = "\
\47|53\n\
\97|13\n\
\97|61\n\
\97|47\n\
\75|29\n\
\61|13\n\
\75|53\n\
\29|13\n\
\97|29\n\
\53|29\n\
\61|53\n\
\97|53\n\
\61|29\n\
\47|13\n\
\75|47\n\
\97|75\n\
\47|61\n\
\75|61\n\
\47|29\n\
\75|13\n\
\53|13\n\
\\n\
\75,47,61,53,29\n\
\97,61,53,29,13\n\
\75,29,13\n\
\75,97,47,61,53\n\
\61,13,29\n\
\97,13,75,29,47\n\
\"

process input = sum $ map middle fixedUpdates
  where [rulesInput, updatesInput] = splitOn "\n\n" input
        rules = map parseRule . lines $  rulesInput
        lookupTables = buildLookupTables rules
        updates = map (map read . splitOn ",") . lines $ updatesInput
        firstPass = zip updates $ map (isValidUpdateOrder lookupTables) updates
        invalidUpdates = map fst $ filter (not . snd) firstPass
        fixedUpdates = map (sortBy (updateOrdering lookupTables)) invalidUpdates

parseRule :: String -> (Int, Int)
parseRule input = (read left, read right)
  where (left, '|':right) = break (== '|') input

isValid (preceedsTable, followsTable) a b
  = b `elem` (preceedsTable ! a) || a `elem` (followsTable ! b)

updateOrdering (preceedsTable, followsTable) a b
  | b `elem` (preceedsTable ! a) = LT
  | a `elem` (followsTable ! b) = LT
  | otherwise = GT

isValidUpdateOrder lookupTables updates
  = and $ zipWith (isValid lookupTables) updates (tail updates)

middle xs = xs !! (length xs `div` 2)

buildLookupTables rules = (preceedsTable, followsTable)
  where min = minimum $ concatMap (\(a, b) -> [a, b]) rules
        max = maximum $ concatMap (\(a, b) -> [a, b]) rules
        baseArray :: Array Int [Int]
        baseArray = listArray (min, max) $ repeat []
        preceedsTable = accum (flip (:)) baseArray rules
        followsTable = accum (flip (:)) baseArray $ map swap rules

swap (a, b) = (b, a)
