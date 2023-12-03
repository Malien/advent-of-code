{-# LANGUAGE NamedFieldPuns #-}
import           Data.Array
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe
import           Data.Ord
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Debug.Trace

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

process input = sum $ map (groupToNum Map.!) $  Set.toList $ accumGroups association symbolsAt
  where State { association, symbolsAt, groupToNum } = divideIntoGroups emptyState input

display = putStrLn . displayGrid . divideIntoGroups emptyState

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

accumGroups association = Set.fromList . concatMap (mapMaybe (association Map.!?) . neighbors)

neighbors (x, y) = [
  (x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
  (x - 1, y),                 (x + 1, y),
  (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]








displayGrid (State { association, pos = (width, height), symbolsAt }) = asString
  where empty = listArray ((0, 0), (height, width - 1)) (repeat ' ')
        grid = foldl (\arr ((x, y), Group id) -> arr // [((y, x), head $ show id)]) empty (Map.toList association)
        withSymbols = foldl (\arr (x, y) -> arr // [((y, x), '*')]) grid symbolsAt
        asString = unlines $ chunksOf width $ elems withSymbols
