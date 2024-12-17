import           Data.Bits

-- This is possibly AoC code of conduct violation (distribution of inputs)
-- 1. It's obfuscated
-- 2. No-one will care
-- 3. My code is hard-speciazed to that input
expected = [2,4,1,3,7,5,4,1,1,3,0,3,5,5,3,0]

main = print . filter ((== expected) . eval []) $ candidates (reverse expected) 0

eval acc a
  | nextA == 0 = reverse (output:acc)
  | otherwise = eval (output:acc) nextA
  where (nextA, output) = step a

step a = let
  b1 = a .&. 0b111
  b2 = b1 `xor` 0b011
  c1 = a `shiftR` b2
  b3 = b2 `xor` c1
  b4 = b3 `xor` 0b011
  a1 = a `shiftR` 3
  output = b4 .&. 0b111
  in (a1, output)

candidates [] prevA = [prevA]
candidates (expected:rest) prevA
  = concatMap (candidates rest)
  . filter ((== expected) . snd . step)
  . map ((prevA `shiftL` 3) .|.)
  $ [0..7]
