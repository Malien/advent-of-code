import           Data.List.Split

main = process <$> readFile "in" >>= print

test = "2-4,6-8\n\
\2-3,4-5\n\
\5-7,7-9\n\
\2-8,3-7\n\
\6-6,4-6\n\
\2-6,4-8"

parseRange :: String -> (Int, Int)
parseRange line = (read start, read end)
  where [start, end] = splitOn "-" line

parse line = (parseRange left, parseRange right)
  where [left, right] = splitOn "," line

disjoint (leftStart, leftEnd) (rightStart, rightEnd) = 
  (leftStart > rightStart && leftEnd > rightStart) || 
  (rightStart > leftStart && rightEnd > rightStart)

process = length . filter not . map (uncurry disjoint . parse) . lines
