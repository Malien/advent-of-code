import           Data.Array      (Array, assocs, elems, (!), (//))
import           Data.Bifunctor  (Bifunctor (bimap))
import           Data.Char       (isSpace)
import           Data.List       (dropWhileEnd, minimumBy)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Ord        (comparing)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Text.Regex.PCRE ((=~))

main = readFile "in" >>= print . process

test = "\
\LR\n\
\\n\
\11A = (11B, XXX)\n\
\11B = (XXX, 11Z)\n\
\11Z = (11B, XXX)\n\
\22A = (22B, XXX)\n\
\22B = (22C, 22C)\n\
\22C = (22Z, 22Z)\n\
\22Z = (22B, 22B)\n\
\XXX = (XXX, XXX)"

-- process input = advance (listArray (1, length start) offsets)
process input = foldl1 lcm offsets
    where (directions, nodes) = parseTask input
          start = startingNodes nodes
          offsets = map (offsetsFor directions (Map.fromList nodes)) start

data Direction = L | R deriving (Show, Eq, Ord)
data Node = Node Char Char Char deriving (Eq, Ord)

instance Show Node where
    show (Node a b c) = [a,b,c]

trimEnd = dropWhileEnd isSpace

parseTask input = (map parseDirection $ trimEnd dir, map parseNodeLine $ lines nodes)
    where [dir, nodes] = splitOn "\n\n" input

parseDirection 'R' = R
parseDirection 'L' = L

isTerminal (Node _ _ 'Z') = True
isTerminal _              = False

isStarting (Node _ _ 'A') = True
isStarting _              = False

parseNodeLine line = (parseNode name, (parseNode left, parseNode right))
    where [[_, name, left, right]] = line =~ "([A-Z1-9]+) = \\(([A-Z1-9]+), ([A-Z1-9]+)\\)" :: [[String]]

parseNode [a,b,c] = Node a b c

startingNodes = filter isStarting . map fst

nextNode node L nodes = fst $ nodes Map.! node
nextNode node R nodes = snd $ nodes Map.! node

getCycle node dirs = bimap (map fst) (map fst) . getCycle' [] Set.empty node (cycle $ zip dirs [0..])

getCycle' path visited node ((dir, pos):dirs) nodes
    | (node, pos) `Set.member` visited = splitWhenOnce (==(node,pos)) $ reverse path
    | otherwise = getCycle' ((node, pos):path) nextVisited nextN dirs nodes
    where nextVisited = Set.insert (node, pos) visited
          nextN = nextNode node dir nodes

splitWhenOnce f xs = splitWhenOnce' [] f xs
    where splitWhenOnce' acc f (x:xs) | f x       = (reverse acc, x:xs)
                                      | otherwise = splitWhenOnce' (x:acc) f xs

terminalSeq acc len (node:nodes) | isTerminal node = terminalSeq (len:acc) (len + 1) nodes
                                 | otherwise       = terminalSeq acc (len + 1) nodes
terminalSeq acc _ [] = acc

cycleCycle prefixPath cyclePath = prefixPath ++ cycle cyclePath

offsetsFor dirs nodes node = terminalAt
    where (prefix, cycle) = getCycle node dirs nodes
          [terminalAt] = terminalSeq [] (length prefix) cycle

