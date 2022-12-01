import           Data.List
import           Data.List.Split
import           Data.Ord

main = process <$> readFile "in" >>= print

process =
    sum
  . take 3
  . sortOn Down
  . map (sum . map read . lines)
  . splitOn "\n\n"
