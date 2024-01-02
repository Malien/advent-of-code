{-# LANGUAGE QuasiQuotes #-}
import           Data.Array        (bounds, listArray, (!))
import qualified Data.Set          as Set
import           Text.RawString.QQ (r)

main = readFile "in" >>= print . process

test = tail [r|
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
|]

process = Set.size . Set.map (\(Ray pos _) -> pos) . amaFirinMahLazor . parseGrid

data Tile = Empty | HSplit | VSplit | FCorner | BCorner

instance Show Tile where
  show Empty   = "."
  show HSplit  = "-"
  show VSplit  = "|"
  show FCorner = "/"
  show BCorner = "\\"

parseTile '.'  = Empty
parseTile '-'  = HSplit
parseTile '|'  = VSplit
parseTile '/'  = FCorner
parseTile '\\' = BCorner

parseGrid inp = listArray ((1,1), (width, height)) $ map parseTile $ filter (/= '\n') inp
  where width = length $ head $ lines inp
        height = length $ lines inp

data Direction = N | E | S | W deriving (Show, Eq, Ord)
data Ray = Ray (Int, Int) Direction deriving (Show, Eq, Ord)

fireRay _ visited ray | Set.member ray visited = visited
fireRay grid visisted ray@(Ray pos dir) = case raySplit of
  OutOfBounds        -> nextVisited
  (Proceed next)     -> fireRay grid nextVisited next
  (Split left right) -> let leftVisited = fireRay grid nextVisited left
                        in  fireRay grid leftVisited right
  where nextVisited = Set.insert ray visisted
        tile = grid ! pos
        raySplit = bounded grid $ nextRay tile ray

amaFirinMahLazor grid = fireRay grid Set.empty (Ray (1, 1) E)

data RaySplit = OutOfBounds | Proceed Ray | Split Ray Ray

nextRay Empty ray           = Proceed $ advance ray
nextRay HSplit (Ray pos N)  = splitRay pos E W
nextRay HSplit (Ray pos S)  = splitRay pos E W
nextRay HSplit ray          = nextRay Empty ray
nextRay VSplit (Ray pos E)  = splitRay pos N S
nextRay VSplit (Ray pos W)  = splitRay pos N S
nextRay VSplit ray          = nextRay Empty ray
nextRay FCorner (Ray pos N) = turn pos E
nextRay FCorner (Ray pos W) = turn pos S
nextRay FCorner (Ray pos S) = turn pos W
nextRay FCorner (Ray pos E) = turn pos N
nextRay BCorner (Ray pos N) = turn pos W
nextRay BCorner (Ray pos E) = turn pos S
nextRay BCorner (Ray pos S) = turn pos E
nextRay BCorner (Ray pos W) = turn pos N

advance (Ray (y, x) N) = Ray (y-1, x) N
advance (Ray (y, x) E) = Ray (y, x+1) E
advance (Ray (y, x) S) = Ray (y+1, x) S
advance (Ray (y, x) W) = Ray (y, x-1) W

splitRay pos l r = Split (advance $ Ray pos l) (advance $ Ray pos r)
turn pos dir = Proceed $ advance $ Ray pos dir

bounded grid (Proceed ray) | not $ inBounds grid ray = OutOfBounds
bounded grid (Split left right) | not $ inBounds grid left = bounded grid (Proceed right)
bounded grid (Split left right) | not $ inBounds grid right = bounded grid (Proceed left)
bounded _ ray = ray

inBounds grid (Ray (x, y) _) = x >= 1 && y >= 1 && x <= width && y <= height
  where ((1,1), (width, height)) = bounds grid
