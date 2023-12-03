{-# LANGUAGE NamedFieldPuns #-}
import           Data.Array      (Ix, array, bounds, elems, listArray, (//))
import           Data.Char       (isDigit)
import           Data.List.Split (chunksOf)
import           Data.Map        (Map, (!), (!?))
import qualified Data.Map        as Map
import           Data.Maybe      (mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set

main = readFile "in" >>= print . process

test = "\
\467..114..\n\
\...*......\n\
\..35..633.\n\
\......#...\n\
\617*......\n\
\.....+.58.\n\
\..592.....\n\
\......755.\n\
\...$.*....\n\
\.664.598.."

process input =
    sum
  $ map (groupToNum !)
  $ neighbouringGroups association symbolsAt
  where State { association, symbolsAt, groupToNum } = divideIntoGroups emptyState input

display = putStrLn . showGrid . divideIntoGroups emptyState

newtype Group = Group Int deriving (Show, Eq, Ord, Ix)
data State = State { association  :: Map (Int, Int) Group
                   , pos          :: (Int, Int)
                   , groupToNum   :: Map Group Int
                   , currentGroup :: Group
                   , symbolsAt    :: [(Int, Int)]
                   } deriving (Show, Eq)

emptyState = State Map.empty (0, 0) Map.empty (Group 0) []

nextGroup (Group id) = Group (id + 1)

divideIntoGroups state@(State { association, currentGroup, pos = (x, y), groupToNum }) str@(ch:_)
  | isDigit ch = divideIntoGroups nextState rest
  where numStr = takeWhile isDigit str
        rest = dropWhile isDigit str
        positions = [(newX, y) | newX <- [x..x + length numStr - 1]]
        num = read numStr
        nextGroups = foldl (\groups pos -> Map.insert pos currentGroup groups) association positions
        nextState = state { association = nextGroups
                          , pos = (x + length numStr, y)
                          , groupToNum = Map.insert currentGroup num groupToNum
                          , currentGroup = nextGroup currentGroup
                          }

divideIntoGroups state@(State { pos = (x, y) }) ('\n':rest)
  = divideIntoGroups (state { pos = (0, y + 1) }) rest

divideIntoGroups state@(State { pos = (x, y) }) ('.':rest)
  = divideIntoGroups (state { pos = (x + 1, y) }) rest

divideIntoGroups state@(State { pos = (x, y), symbolsAt }) (_:rest)
  = divideIntoGroups (state { pos = (x + 1, y) , symbolsAt = (x, y) : symbolsAt }) rest

divideIntoGroups state [] = state

neighbouringGroups :: Map (Int, Int) Group -> [(Int, Int)] -> [Group]
neighbouringGroups association = uniq . concatMap (mapMaybe (association !?) . neighbors)

uniq = Set.toList . Set.fromList

neighbors (x, y) = [
  (x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
  (x - 1, y),                 (x + 1, y),
  (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]






-- # =========== #
-- # Debug stuff #
-- # =========== #

showGrid (State { association, pos = (width, height), symbolsAt }) =
    gridToString
  $ drawSymbols symbolsAt
  $ drawGroups association
  $ emptyGrid (width - 1) height

drawGroups association
  = (// [((y, x), head $ show group) | ((x, y), Group group) <- Map.toList association])

drawSymbols symbolsAt = (// [((y, x), '*') | (x, y) <- symbolsAt])

gridToString arr = unlines $ chunksOf width $ elems arr
  where (_, (_, width)) = bounds arr

emptyGrid width height = listArray ((0, 0), (height, width)) (repeat ' ')
