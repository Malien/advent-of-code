import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Int (Int64)

main = readFile "input.txt" >>= print . solution

solution str = 
    result .
    Map.insertWith (+) (head input) 1 .
    countChars .
    (!!40) .
    polymerizations transforms .
    collectPairs $ input
    where (input: (_: trList)) = lines str
          transforms = parseTransformChain trList

polymerizations transforms = iterate (applyPolymerization transforms)

parseTransformChain = Map.fromList . map parseTransformLink

parseTransformLink :: String -> ((Char, Char), Char)
parseTransformLink str = ((left, right), to)
    where [left, right] = take 2 str
          to = head $ drop 6 str

collectPairs str = stats $ zip str (tail str)

applyPolymerization transforms = 
    foldl (\map (pair, count) -> Map.insertWith (+) pair count map) Map.empty . 
    concatMap (transformPair transforms) . 
    Map.toList

transformPair trsfrm (pair, count) = [((fst pair, new), count), ((new, snd pair), count)]
    where new = trsfrm ! pair

countChars :: Map (Char, Char) Int64 -> Map Char Int64
countChars = 
    foldl (\map (a, count) -> Map.insertWith (+) a count map) Map.empty . 
    map (\((_, a), count) -> (a, count)) . 
    Map.toList

stats :: Ord a => [a] -> Map a Int64
stats = foldl (\map char -> Map.insertWith (+) char 1 map) Map.empty

result xs = maximum xs - minimum xs

