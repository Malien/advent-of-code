{-# LANGUAGE QuasiQuotes #-}

import           Data.Bits         (Bits (shiftL, shiftR, xor, (.&.)))
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe        (fromMaybe)
import           Text.RawString.QQ (r)

main = readFile "in" >>= print . process

test = tail [r|
1
2
3
2024
|]

-- It's off by one on real input ¯\_(ツ)_/¯ 
process
  = maximizeSeq
  . map (cached . preceeders . take 2000 . diffs . read)
  . lines

maximizeSeq caches = maximum $ map (\seq -> sum $ map (numForSeq seq) caches) seqs

numForSeq seq cache  = fromMaybe 0 $ Map.lookup seq cache

seqs :: [(Int, Int, Int, Int)]
seqs = [(a,b,c,d) | a <- [-9..9], b <- [-9..9], c <- [-9..9], d <- [-9..9]]

cached = cached' Map.empty
cached' cache ((x,preceeds):rest) = cached' (Map.insertWith (\_ old -> old) preceeds x cache) rest
cached' cache [] = cache

preceeders = preceeders' []
preceeders' acc (a:b:c:d:rest) = preceeders' ((fst d, (snd a, snd b, snd c, snd d)):acc) (b:c:d:rest)
preceeders' acc _ = acc

diffs start = zipWith (\prev next -> (prev, prev - next)) (tail $ digits start) (digits start)
digits = map (`mod` 10) . secrets
secrets = iterate secret

secret s0 = s3
  where s1 = (s0 `xor` (s0 `shiftL` 6 )) .&. 0xFFFFFF
        s2 = (s1 `xor` (s1 `shiftR` 5 )) .&. 0xFFFFFF
        s3 = (s2 `xor` (s2 `shiftL` 11)) .&. 0xFFFFFF
