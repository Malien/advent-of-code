{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE QuasiQuotes           #-}

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
        compacted = compact (reverse cells) cells


expand (Occupied { size, blockID = (BlockID id) }) = replicate size id
expand (Blank { size })                            = replicate size 0

parsePair id [a] = [Occupied { size = digitToInt a, blockID = id }]
parsePair id [a, '0'] = [Occupied { size = digitToInt a, blockID = id }]
parsePair id [a, b] = [Occupied { size = digitToInt a, blockID = id }, Blank { size = digitToInt b }]

compact (Blank _:uncompactedCells) compactedCells
  = compact uncompactedCells compactedCells
compact (Occupied { size, blockID }:uncompactedCells) compactedCells
  = compact uncompactedCells $ slotIn [] (blockID, size) compactedCells
compact [] compactedCells = compactedCells

slotIn :: [Cell] -> (BlockID, Int) -> [Cell] -> [Cell]
slotIn acc (targetID, size) (Occupied { blockID }:rest)
  | targetID == blockID = reverse (Occupied { size, blockID } : acc) ++ removeCell [] targetID rest
slotIn acc target (cell@Occupied { }:rest) = slotIn (cell:acc) target rest
slotIn acc (blockID, size) (Blank freeSize:rest)
  | freeSize == size = reverse (Occupied { size, blockID } : acc) ++ removeCell [] blockID rest
  | freeSize < size = slotIn (Blank freeSize:acc) (blockID, size) rest
  | freeSize > size = reverse (Blank (freeSize - size) : Occupied { size, blockID } : acc) ++ removeCell [] blockID rest

removeCell acc targetID (Blank a:Occupied { blockID, size }:Blank b:rest)
  | blockID == targetID = reverse (Blank (a + size + b) : acc) ++ rest
removeCell acc targetID (Blank a:Occupied { blockID, size }:rest)
  | blockID == targetID = reverse (Blank (a + size) : acc) ++ rest
removeCell acc targetID (Occupied { blockID, size }:Blank a:rest)
  | blockID == targetID = reverse (Blank (a + size) : acc) ++ rest
removeCell acc targetID (Occupied { blockID, size }:rest)
  | blockID == targetID = reverse (Blank size : acc) ++ rest
removeCell acc targetID (cell:rest) = removeCell (cell:acc) targetID rest
removeCell acc targetID [] = reverse acc
