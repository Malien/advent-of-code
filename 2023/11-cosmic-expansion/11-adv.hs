import           Data.Array (Array, assocs, bounds, listArray)
import           Data.Set   (Set)
import qualified Data.Set   as Set

main = readFile "in" >>= print . process

test = "\
\...#......\n\
\.......#..\n\
\#.........\n\
\..........\n\
\......#...\n\
\.#........\n\
\.........#\n\
\..........\n\
\.......#..\n\
\#...#....."

process = 
    (`div` 2) 
  . sum 
  . lenBetweenGalaxies 
  . expandUniverse
  . parseEarlyUniverse

parseEarlyUniverse input = 
  listArray ((0,0),(width-1,height-1)) $ filter (/= '\n') input
  where width = length $ head $ lines input
        height = length $ lines input

galaxies universe = [pos | (pos, tile) <- assocs universe, tile == '#']

columns universe = [minx..maxx]
  where ((miny, minx), (maxy, maxx)) = bounds universe

rows universe = [miny..maxy]
  where ((miny, minx), (maxy, maxx)) = bounds universe

rowsWithGalaxies = Set.fromList . map fst . galaxies
colsWithGalaxies = Set.fromList . map snd . galaxies

emptyRows universe = filter (not . (`Set.member` withGalaxies)) $ rows universe
  where withGalaxies = rowsWithGalaxies universe

emptyCols universe = filter (not . (`Set.member` withGalaxies)) $ columns universe
  where withGalaxies = colsWithGalaxies universe

expandUniverse universe = map (scewX . scewY) $ galaxies universe
  where scewX (y, x) = (y, x + 999_999 * length (filter (< x) $ emptyCols universe))
        scewY (y, x) = (y + 999_999 * length ( filter (< y) $ emptyRows universe), x)

lenBetweenGalaxies galaxies = [len a b | a <- galaxies, b <- galaxies, a /= b]

len (y1, x1) (y2, x2) = abs (y1 - y2) + abs (x1 - x2)
