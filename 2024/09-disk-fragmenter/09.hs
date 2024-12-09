{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE QuasiQuotes           #-}

import           Data.Array        (Array, listArray, (!))
import           Data.Char         (digitToInt)
import           Data.List.Split   (chunksOf)
import           Text.RawString.QQ (r)

main = readFile "in" >>= print . process

test = tail [r|
2333133121414131402
|]

test2 = tail [r|
12345
|]

newtype BlockID = BlockID Int deriving (Show, Eq, Ord, Enum)
data Cell = Blank { size :: Int } | Occupied { size :: Int, blockID :: BlockID } deriving (Show)

process input = sum $ zipWith (*) [0..] $ concatMap expand compacted
  where cells = concat . zipWith parsePair [(BlockID 0)..] . chunksOf 2 . init $ input
        cellsArray = listArray (1, length cells) cells
        compacted = reverse $ compact CompactState {
          leftIdx = 1,
          leftCell = cellsArray ! 1,
          rightIdx = length cells,
          rightCell = cellsArray ! length cells,
          acc = [],
          strip = cellsArray
        }


expand (BlockID id, size) = replicate size id

parsePair id [a] = [Occupied { size = digitToInt a, blockID = id }]
parsePair id [a, '0'] = [Occupied { size = digitToInt a, blockID = id }]
parsePair id [a, b] = [Occupied { size = digitToInt a, blockID = id }, Blank { size = digitToInt b }]

data CompactState = CompactState {
  leftIdx   :: Int,
  leftCell  :: Cell,
  rightIdx  :: Int,
  rightCell :: Cell,
  acc       :: [(BlockID, Int)],
  strip     :: Array Int Cell
}

compact state@(CompactState { leftIdx, rightIdx, rightCell = Blank _, acc })
  | leftIdx == rightIdx = acc
compact state@(CompactState { leftIdx, rightIdx, rightCell = Occupied { size, blockID }, acc })
  | leftIdx == rightIdx = (blockID, size):acc
compact state@(CompactState { leftCell = Occupied { size, blockID } }) = compact state {
  leftIdx   = state.leftIdx + 1,
  leftCell  = state.strip ! (state.leftIdx + 1),
  acc       = (blockID, size) : state.acc
}
compact state@(CompactState { rightCell = Blank _ }) =
  compact state {
  rightIdx  = state.rightIdx - 1,
  rightCell = state.strip ! (state.rightIdx - 1)
}
compact state@(CompactState {
  leftCell  = Blank { size = freeSize },
  rightCell = Occupied { size, blockID }
})
  | freeSize == size = compact state {
      leftIdx   = state.leftIdx + 1,
      leftCell  = state.strip ! (state.leftIdx + 1),
      rightIdx  = state.rightIdx - 1,
      rightCell = state.strip ! (state.rightIdx - 1),
      acc       = (blockID, size) : state.acc
    }
  | freeSize < size = compact state {
      leftIdx   = state.leftIdx + 1,
      leftCell  = state.strip ! (state.leftIdx + 1),
      rightIdx  = state.rightIdx,
      rightCell = Occupied { size = size - freeSize, blockID },
      acc       = (blockID, freeSize) : state.acc
    }
  | freeSize > size = compact state {
      leftIdx   = state.leftIdx,
      leftCell  = Blank { size = freeSize - size },
      rightIdx  = state.rightIdx - 1,
      rightCell = state.strip ! (state.rightIdx - 1),
      acc       = (blockID, size) : state.acc
    }
