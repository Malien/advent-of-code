import           Data.List.Split

main = process <$> readFile "in" >>= print

process =
    maximum
  . map (sum . map read . lines)
  . splitOn "\n\n"
