import           Data.List
import           Data.List.Split
import           Data.Ord
import           Data.Char
import           Data.Maybe
import           Debug.Trace
import           Text.Regex.PCRE
import           Data.Array
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

main = readFile "in" >>= print . process

process inp = sum $ map (countSides grid) $ Set.toList grid
  where grid = Set.fromList . map parseCube . lines $ inp

countSides grid = length . filter not . map (`Set.member` grid) . neighbors

neighbors point = map (^^+ point) $ concatMap (`map` [-1,1]) [(,0,0), (0,,0), (0,0,)]

(x,y,z) ^^+ (a,b,c) = (x+a, y+b, z+c)

parseCube line = (x, y, z) :: (Int, Int, Int)
  where [x,y,z] = map read $ splitOn "," line

test = 
  "2,2,2\n\
  \1,2,2\n\
  \3,2,2\n\
  \2,1,2\n\
  \2,3,2\n\
  \2,2,1\n\
  \2,2,3\n\
  \2,2,4\n\
  \2,2,6\n\
  \1,2,5\n\
  \3,2,5\n\
  \2,1,5\n\
  \2,3,5"
