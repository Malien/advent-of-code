{-# LANGUAGE QuasiQuotes #-}

import           Data.Array
import           Data.List         (find)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Sequence     (Seq ((:<|)), (><), (|>))
import qualified Data.Sequence     as Seq
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Text.RawString.QQ (r)

main = readFile "in" >>= print . process

test = tail [r|
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
|]

process input = length results
  where (grid, start, end) = parse input
        results = cheatingBfs grid end 49 baselineTraversal Set.empty [] (Seq.singleton (start, 0, Virgin))
        baselineTraversal = noCheatingBfs grid end Map.empty (Seq.singleton (start, 0))

count :: Ord a => [a] -> Map a Int
count = foldr (\value map -> Map.insertWith (+) value 1 map) Map.empty

data Cell = Wall | Blank deriving Eq
data State = Virgin | Cheating PathLength Coord | DoneCheating Coord Coord deriving (Show, Eq, Ord)
type Coord = (Int, Int)
type PathLength = Int
type Baseline = Map Coord PathLength
type Grid = Array Coord Cell

noCheatingBfs ::
  Grid ->
  Coord ->
  Baseline ->
  Seq (Coord, PathLength) ->
  Baseline
noCheatingBfs grid end visited ((pos, length):<|rest)
  | pos == end = noCheatingBfs grid end (Map.insert pos length visited) rest
  | pos `Map.member` visited = noCheatingBfs grid end visited rest
  | otherwise = noCheatingBfs grid end (Map.insert pos length visited) (rest >< Seq.fromList next)
  where next =
          [ (neighbor, length + 1)
          | neighbor <- neighbors pos
          , inRange (bounds grid) neighbor
          , grid ! neighbor == Blank
          , neighbor `Map.notMember` visited ]
noCheatingBfs grid end visited Seq.Empty = visited

cheatingBfs ::
  Grid ->
  Coord ->
  Int ->
  Baseline ->
  Set (Coord, State) ->
  [(PathLength, State)] ->
  Seq (Coord, PathLength, State) ->
  [(PathLength, State)]
cheatingBfs grid end magnitude baseline visited acc ((pos, length, state):<|rest)
  | isWorseThanBaseline baseline magnitude pos length state = continue visited acc rest
  | length >= baseline Map.! end = continue (Set.insert (pos, state) visited) acc rest
  | (pos, state) `Set.member` visited = continue visited acc rest
  | pos == end = continue visited ((length, state):acc) rest
  | otherwise = continue (Set.insert (pos, state) visited) acc (rest >< Seq.fromList next)
  where continue = cheatingBfs grid end magnitude baseline
        next =
          [ (neighbor, length + 1, state')
          | neighbor <- neighbors pos
          , inRange (bounds grid) neighbor
          , state' <- nextState state (grid ! neighbor) neighbor
          , (neighbor, state') `Set.notMember` visited]

cheatingBfs _ _ _ _ _ acc Seq.Empty = acc

isWorseThanBaseline :: Baseline -> Int ->  Coord -> PathLength -> State -> Bool
isWorseThanBaseline _ _ _ _ Virgin = False
isWorseThanBaseline _ _ _ _ (Cheating _ _) = False
isWorseThanBaseline baseline magnitude pos length (DoneCheating _ _) = length + magnitude >= baseline Map.! pos

neighbors (y, x) = [(y+1, x), (y, x+1), (y-1, x), (y, x-1)]

nextState    Virgin              Blank _   = [Virgin]
nextState    Virgin              Wall  pos = [Cheating 1 pos]
nextState   (Cheating len _)     Wall  _   | len >= 20 = []
nextState   (Cheating len start) Wall  _   = [Cheating (len + 1) start]
nextState   (Cheating len start) Blank pos | len >= 20 = [DoneCheating start pos]
nextState   (Cheating len start) Blank pos = [Cheating (len + 1) start, DoneCheating start pos]
nextState s@(DoneCheating _ _)   Blank _   = [s]
nextState   (DoneCheating _ _)   Wall  _   = []
-- nextState   (CheatedOnce prev) Blank pos = Just $ CheatedTwice prev pos
-- nextState   (CheatedOnce _   ) Wall  pos = Nothing
-- nextState s@(CheatedTwice _ _) Blank _   = Just s
-- nextState   (CheatedTwice _ _) Wall  _   = Nothing

parse input = (grid, start, end)
  where width = length $ head $ lines input
        height = length $ lines input
        grid = listArray ((1, 1), (height, width)) $ map parseCell $ filter (/= '\n') input
        Just (start, 'S') = find ((== 'S') . snd) $ zip (range $ bounds grid) $ filter (/= '\n') input
        Just (end  , 'E') = find ((== 'E') . snd) $ zip (range $ bounds grid) $ filter (/= '\n') input

parseCell '#' = Wall
parseCell '.' = Blank
parseCell 'S' = Blank
parseCell 'E' = Blank

instance Show Cell where
  show Wall  = "#"
  show Blank = "."
