import           Data.Array      (Array, accum, array, listArray)
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
        updates = map (map read . splitOn ",") . lines $ updatesInput
        firstPass = zip updates $ map (isValidUpdateOrder rules) updates
        invalidUpdates = map fst $ filter (not . snd) firstPass
        fixedUpdates = map (sortBy (updateOrdering rules)) invalidUpdates

parseRule :: String -> (Int, Int)
parseRule input = (read left, read right)
  where (left, '|':right) = break (== '|') input

isValid (preceeds, follows) (a, b)
  | a == preceeds && b == follows = True
  | a == follows && b == preceeds = False
  | otherwise = True

isValidUpdateOrder rules update = and [isValid rule update | update <- zip update (tail update), rule <- rules]

middle xs = xs !! (length xs `div` 2)

updateOrdering ((preceeds, follows):rules) a b
  | a == preceeds && b == follows = LT
  | a == follows && b == preceeds = GT
  | otherwise = updateOrdering rules a b
updateOrdering [] _ _ = EQ

buildLookupTable rules = array (min, max) rules
  where min = minimum $ map fst rules
        max = maximum $ map fst rules

buildLookupTables rules = (preceedsTable, followsTable)
  where min = minimum $ concatMap (\(a, b) -> [a, b]) rules
        max = maximum $ concatMap (\(a, b) -> [a, b]) rules
        baseArray :: Array Int [Int]
        baseArray = listArray (min, max) $ repeat []
        preceedsTable = accum (flip (:)) baseArray rules
        followsTable = accum (flip (:)) baseArray $ map swap rules

swap (a, b) = (b, a)
