import           Data.Char       (isDigit)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

main = readFile "in" >>= print . process

process =
  sum
  . map fst
  . filter snd
  . zip [1..]
  . map (uncurry (<) . parsePacketPair)
  . splitOn "\n\n"

data Packet = List [Packet] | Item Int deriving Eq

instance Show Packet where
  show (Item x)    = show x
  show (List list) = show list

instance Ord Packet where
  compare (Item x) (Item y) = compare x y
  compare (List []) (List []) = EQ
  compare (List _) (List []) = GT
  compare (List []) (List _) = LT
  compare (List (x:xs)) (List (y:ys)) = case compare x y of
    EQ -> compare (List xs) (List ys)
    LT -> LT
    GT -> GT
  compare (List x) (Item y) = compare (List x) (List [Item y])
  compare (Item x) (List y) = compare (List [Item x]) (List y)

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
