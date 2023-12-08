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

test = "\
\RL\n\
\\n\
\AAA = (BBB, CCC)\n\
\BBB = (DDD, EEE)\n\
\CCC = (ZZZ, GGG)\n\
\DDD = (DDD, DDD)\n\
\EEE = (EEE, EEE)\n\
\GGG = (GGG, GGG)\n\
\ZZZ = (ZZZ, ZZZ)"

test2 = "\
\LLR\n\
\\n\
\AAA = (BBB, BBB)\n\
\BBB = (AAA, ZZZ)\n\
\ZZZ = (ZZZ, ZZZ)"

process input = traversal "AAA" 0 (cycle directions) $ Map.fromList nodes
    where (directions, nodes) = parseTask input

data Direction = L | R deriving (Show, Eq, Ord)

trimEnd = dropWhileEnd isSpace

parseTask input = (map parseDirection $ trimEnd dir, map parseNode $ lines nodes)
    where [dir, nodes] = splitOn "\n\n" input

parseDirection 'R' = R
parseDirection 'L' = L

parseNode :: String -> (String, (String, String))
parseNode line = (name, (left, right))
    where [[_, name, left, right]] = line =~ "([A-Z]+) = \\(([A-Z]+), ([A-Z]+)\\)" :: [[String]]

traversal "ZZZ" steps _ _ = steps
traversal node steps (L:directions) nodes = traversal left (steps + 1) directions nodes
    where (left, _) = nodes Map.! node
traversal node steps (R:directions) nodes = traversal right (steps + 1) directions nodes
    where (_, right) = nodes Map.! node
