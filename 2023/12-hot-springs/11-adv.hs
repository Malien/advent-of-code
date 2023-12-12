import           Data.Array      (array, (!))
import           Data.List       (intercalate)
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

process = sum . map (uncurry fitGroupsMemo . uncurry unfold . parseLine) . lines

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

unfold springs groups = (intercalate [Unknown] $ replicate 5 springs, concat $ replicate 5 groups)

fitGroupsMemo :: [Spring] -> [Int] -> Int
fitGroupsMemo springs groups = fitGroups springs groups
  where sLen = length springs
        gLen = length groups
        -- ama be honest. I stole this lazy array memoization trick from
        -- https://github.com/ColonelPhantom/aoc2023/blob/6ac34dbb4f21bc0c349a0b06d4b1d482e3655f6e/Day12.hs
        -- My solution is different enough that I don't feel THAT bad about it.
        -- Since haskell does it's lazy thing via thunks, array is by default filled with those thunks.
        -- Once we access a value, it is computed, and stored intead of the thunk. Brilliant.
        -- And instead of Map ([Spring], [Group]) Int, we can use an Array (Int, Int) Int
        -- where the indices are the lengths of the springs and groups lists.
        -- This is equivalent, since in a given sequence of springs and groups, the pair
        -- of sublist lengths map to unique ([Spring], [Group]) pair.
        memo = array ((0, 0), (length springs, length groups))
          [ ((sl, gl), fitGroupsBase (drop sl springs) (drop gl groups))
          | sl <- [0..sLen]
          , gl <- [0..gLen]
          ]

        fitGroups springs groups = memo ! (sLen - length springs, gLen - length groups)

        fitGroupsBase :: [Spring] -> [Int] -> Int
        fitGroupsBase springs []     | Broken `notElem` springs = 1
                                     | otherwise                = 0
        fitGroupsBase []      groups = 0
        fitGroupsBase (Fixed:springs) groups = fitGroups springs groups
        fitGroupsBase (Broken:springs) (group:groups) = case tryFit (Broken:springs) group of
          Just left -> fitGroups left groups
          Nothing   -> 0
        fitGroupsBase (Unknown:springs) (group:groups) = case tryFit (Unknown:springs) group of
          Just left -> fitGroups left groups + fitGroups springs (group:groups)
          Nothing   -> fitGroups springs (group:groups)
