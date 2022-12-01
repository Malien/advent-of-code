import           Data.List
import           Data.List.Split
import           Data.Ord

main = process <$> readFile "in" >>= print

process =
    maximum
  . map (sum . map read . lines)
  . splitOn "\n\n"
