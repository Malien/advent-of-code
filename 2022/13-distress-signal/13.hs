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
  sum
  . map fst
  . filter ((== Correct) . snd)
  .  zip [1..] 
  . map (uncurry isInCorrectOrder . parsePacketPair) 
  . splitOn "\n\n"

data Packet = List [Packet] | Item Int

instance Show Packet where
  show (Item x) = show x
  show (List list) = show list

parsePacketPair def = (parsePacket left, parsePacket right)
  where [left, right] = lines def
        

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

isInCorrectOrder (Item x) (Item y) | x > y = Incorrect
                                   | x < y = Correct
                                   | otherwise = Undetermined
isInCorrectOrder (List []) (List []) = Undetermined
isInCorrectOrder (List _) (List []) = Incorrect
isInCorrectOrder (List []) (List _) = Correct
isInCorrectOrder (List (x:xs)) (List (y:ys)) = case isInCorrectOrder x y of
  Undetermined -> isInCorrectOrder (List xs) (List ys)
  Correct -> Correct
  Incorrect -> Incorrect
isInCorrectOrder (List x) (Item y) = isInCorrectOrder (List x) (List [Item y])
isInCorrectOrder (Item x) (List y) = isInCorrectOrder (List [Item x]) (List y)

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
