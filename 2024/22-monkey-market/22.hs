{-# LANGUAGE QuasiQuotes #-}

import           Data.Bits         (Bits (shiftL, shiftR, xor, (.&.)))
import           Text.RawString.QQ (r)

main = readFile "in" >>= print . process

test = tail [r|
1
10
100
2024
|]

process = sum . map ((!!2000) . iterate secret . read) . lines

secret :: Int -> Int
secret x = step3
  where step1 = (x     `xor` (x     `shiftL` 6 )) .&. 0xFFFFFF
        step2 = (step1 `xor` (step1 `shiftR` 5 )) .&. 0xFFFFFF
        step3 = (step2 `xor` (step2 `shiftL` 11)) .&. 0xFFFFFF
