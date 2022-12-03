import           Data.Char
import qualified Data.Set  as Set

main = process <$> readFile "in" >>= print

test = "vJrwpWtwJgWrhcsFMMfFFhFp\n\
\jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
\PmmdzqPrVvPwwTWBwg|\n\
\wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
\ttgJtRGJQctTZtZT\n\
\CrZsJsPPZsGzwwsLwLmpwMDw"

compartments list = splitAt (length list `div` 2) list

common left right = Set.fromList left `Set.intersection` Set.fromList right

priority char | ord char < ord 'a' = ord char - ord 'A' + 27
              | otherwise          = ord char - ord 'a' + 1

fromSingleton set = case Set.toList set of [x] -> x

process =
    sum
  . map (priority . fromSingleton . uncurry common . compartments)
  . lines
