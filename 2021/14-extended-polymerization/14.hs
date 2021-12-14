import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Int (Int64)

main = readFile "input.txt" >>= print . solution

solution str = result . Map.elems . stats . (!!10) . polymerizations transforms $ input
    where (input: (_: trList)) = lines str
          transforms = parseTransformChain trList

polymerizations transforms = iterate (applyPolymerization transforms)

parseTransformChain = Map.fromList . map parseTransformLink

parseTransformLink :: String -> ((Char, Char), Char)
parseTransformLink str = ((left, right), to)
    where [left, right] = take 2 str
          to = head $ drop 6 str

applyPolymerization transforms str = foldl accumTransforms [head str] $ zip str (tail str)
    where accumTransforms acc pair = acc ++ [transforms!pair, snd pair]

stats :: Ord a => [a] -> Map a Int64
stats = foldl (\map char -> Map.insertWith (+) char 1 map) Map.empty

result xs = maximum xs - minimum xs

