import qualified Data.Set as Set

main = process <$> readFile "in" >>= print

test = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

unique ls = Set.size (Set.fromList ls) == length ls

windows size list@(_:xs) = take size list : windows size xs

process = fst . head . filter (unique . snd) . zip [4..] . windows 4
