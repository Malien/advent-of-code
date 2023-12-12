import           Data.List.Split (splitOn)

main = readFile "in" >>= print . process

test = "\
\???.### 1,1,3\n\
\.??..??...?##. 1,1,3\n\
\?#?#?#?#?#?#?#? 1,3,1,6\n\
\????.#...#... 4,1,1\n\
\????.######..#####. 1,6,5\n\
\?###???????? 3,2,1"

test2 = "??.#???..???.#? 1,2,1,1,1"

process = sum . map (uncurry fitGroups . parseLine) . lines

data Spring = Fixed | Broken | Unknown deriving Eq
instance Show Spring where
  show Fixed   = "."
  show Broken  = "#"
  show Unknown = "?"

parseLine :: String -> ([Spring], [Int])
parseLine line = (map parseSpring springs, map read $ splitOn "," groups)
  where [springs, groups] = words line

parseSpring '.' = Fixed
parseSpring '#' = Broken
parseSpring '?' = Unknown

tryFit []                0     = Just []
tryFit []                _     = Nothing
tryFit (Fixed  :left   ) 0     = Just left
tryFit (Broken :left   ) 0     = Nothing
tryFit (Unknown:left   ) 0     = Just left
tryFit (Fixed  :springs) group = Nothing
tryFit (Broken :springs) group = tryFit springs (group - 1)
tryFit (Unknown:springs) group = tryFit springs (group - 1)

fitGroups :: [Spring] -> [Int] -> Int
fitGroups springs []     | Broken `notElem` springs = 1
                         | otherwise                = 0
fitGroups []      groups = 0
fitGroups (Fixed:springs) groups = fitGroups springs groups
fitGroups (Broken:springs) (group:groups) = case tryFit (Broken:springs) group of
  Just left -> fitGroups left groups
  Nothing   -> 0
fitGroups (Unknown:springs) (group:groups) = case tryFit (Unknown:springs) group of
  Just left -> fitGroups left groups + fitGroups springs (group:groups)
  Nothing   -> fitGroups springs (group:groups)
