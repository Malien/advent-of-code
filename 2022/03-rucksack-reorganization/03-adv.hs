import           Data.Char
import           Data.List.Split
import qualified Data.Set        as Set

main = process <$> readFile "in" >>= print

test = "vJrwpWtwJgWrhcsFMMfFFhFp\n\
\jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
\PmmdzqPrVvPwwTWBwg|\n\
\wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
\ttgJtRGJQctTZtZT\n\
\CrZsJsPPZsGzwwsLwLmpwMDw"

priority char | ord char < ord 'a' = ord char - ord 'A' + 27
              | otherwise          = ord char - ord 'a' + 1

fromSingleton set = case Set.toList set of [x] -> x

process =
    sum
  . map (priority . fromSingleton . foldr1 Set.intersection)
  . chunksOf 3
  . map Set.fromList
  . lines
