import           Data.List
import           Data.List.Split
import           Data.Ord
import           Data.Char
import           Data.Maybe
import           Debug.Trace
import           Text.Regex.PCRE
import           Data.Array
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

main = process <$> readFile "in" >>= print

process = 
  findSplitters
  . orderPackets
  . (++splitters)
  . map parsePacket
  . filter (not . null)
  . lines 
  . unlines
  . splitOn "\n\n"

findSplitters packets = product $ map ((1 +) . fromJust . findPacketIdx packets) splitters

findPacketIdx packets packet = findIndex (== packet) packets

data Packet = List [Packet] | Item Int deriving Eq

instance Show Packet where
  show (Item x) = show x
  show (List list) = show list

parsePacket inp = packet
  where ([packet], "") = parsePacket' [] inp
        parsePacket' acc []         = (reverse acc, [])
        parsePacket' acc (']':rest) = (reverse acc, rest)
        parsePacket' acc ('[':rest) = parsePacket' (List item : acc) left 
          where (item, left) = parsePacket' [] rest
        parsePacket' acc (',':rest) = parsePacket' acc rest
        parsePacket' acc line       = parsePacket' newAcc afterNumber
          where afterNumber = dropWhile isDigit line
                beforeNumber = takeWhile isDigit line
                newAcc = Item (read $ takeWhile isDigit line) : acc

data PacketOrdering = Correct | Incorrect | Undetermined deriving (Show, Eq)

packetOrdering (Item x) (Item y) | x > y = Incorrect
                                 | x < y = Correct
                                 | otherwise = Undetermined
packetOrdering (List []) (List []) = Undetermined
packetOrdering (List _) (List []) = Incorrect
packetOrdering (List []) (List _) = Correct
packetOrdering (List (x:xs)) (List (y:ys)) = case packetOrdering x y of
  Undetermined -> packetOrdering (List xs) (List ys)
  Correct -> Correct
  Incorrect -> Incorrect
packetOrdering (List x) (Item y) = packetOrdering (List x) (List [Item y])
packetOrdering (Item x) (List y) = packetOrdering (List [Item x]) (List y)

splitters = [List [List [Item 2]], List [List [Item 6]]]

orderPackets [] = []
orderPackets [x] = [x]
orderPackets packets = combineBuckets (orderPackets left) (orderPackets right)
  where (left, right) = splitAt (length packets `div` 2) packets

-- orderingRound [] = []
-- orderingRound (x:xs) = bucket : orderingRound left
--   where (bucket, left) = combineBuckets [] x xs

-- combineBuckets acc item [] = (item, acc)
-- combineBuckets acc item (x:xs) = case packetOrdering (last item) (head x) of
--   Correct -> (item ++ x, acc ++ xs)
--   Incorrect -> combineBuckets (x:acc) item xs

combineBuckets [] [] = []
combineBuckets xs [] = xs
combineBuckets [] ys = ys
combineBuckets (x:xs) (y:ys) = case packetOrdering x y of
  Correct -> x : combineBuckets xs (y:ys)
  Incorrect -> y : combineBuckets (x:xs) ys

test = 
  "[1,1,3,1,1]\n\
  \[1,1,5,1,1]\n\
  \\n\
  \[[1],[2,3,4]]\n\
  \[[1],4]\n\
  \\n\
  \[9]\n\
  \[[8,7,6]]\n\
  \\n\
  \[[4,4],4,4]\n\
  \[[4,4],4,4,4]\n\
  \\n\
  \[7,7,7,7]\n\
  \[7,7,7]\n\
  \\n\
  \[]\n\
  \[3]\n\
  \\n\
  \[[[]]]\n\
  \[[]]\n\
  \\n\
  \[1,[2,[3,[4,[5,6,7]]]],8,9]\n\
  \[1,[2,[3,[4,[5,6,0]]]],8,9]"
