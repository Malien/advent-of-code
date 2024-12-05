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

process input = sum $ map fst $ filter snd $ zip (map middle updates) $ map (isValidUpdateOrder rules) updates
  where [rulesInput, updatesInput] = splitOn "\n\n" input
        rules = map parseRule . lines $  rulesInput
        updates :: [[Int]]
        updates = map (map read . splitOn ",") . lines $ updatesInput

parseRule :: String -> (Int, Int)
parseRule input = (read left, read right)
  where (left, '|':right) = break (== '|') input

isValid (preceeds, follows) (a, b)
  | a == preceeds && b == follows = True
  | a == follows && b == preceeds = False
  | otherwise = True

isValidUpdateOrder rules update = and [isValid rule update | update <- zip update (tail update), rule <- rules]

middle xs = xs !! (length xs `div` 2)
