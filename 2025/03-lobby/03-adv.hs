{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TupleSections #-}

import           Data.Char         (digitToInt)
import           Data.List         (maximumBy)
import           Data.Ord          (comparing)
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Text.RawString.QQ (r)

main = readFile "in" >>= print . process

test = tail [r|
987654321111111
811111111111119
234234234234278
818181911112111
|]

findBestMatch l xs = (Set.insert v l, Set.delete v xs)
  where (v, _) = maximumBy (comparing snd) [(x,) $ accumDecimal $ Set.insert x l| x <- Set.toList xs]

accumDecimal = foldl (\acc (_, x) -> acc * 10 + x) 0

solve
  = accumDecimal
  . fst
  . (!!12)
  . iterate (uncurry findBestMatch)
  . (Set.empty,)
  . Set.fromList
  . zip [0..]
  . map digitToInt

process = sum . map solve . lines
