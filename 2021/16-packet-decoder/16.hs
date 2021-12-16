{-# LANGUAGE TupleSections #-}

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.Binary.Strict.Get as G
import qualified Data.Binary.Strict.BitGet as BG
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BLB
import qualified Data.ByteString.Lazy.Builder.ASCII as ASCII
import qualified Data.Text.Encoding as E
import Text.Hex (decodeHex)
import Data.Text (pack)
import Data.Maybe (fromJust)
import Data.Bits (shiftL, shiftR)
import Debug.Trace

main = readFile "input.txt" >>= print . solution . filter (/= '\n')

data Header = Header
    { version :: Word8
    , typeID  :: Word8
    } deriving Show

data PacketBody = Literal Word64 | Operator [Packet] deriving Show

data Packet = Packet Header PacketBody deriving Show

solution = eval . fst . fromRight . (`BG.runBitGet` parsePacket) . toByteString

sumVersionNumbers :: Packet -> Int
sumVersionNumbers (Packet header (Literal _)) = fromIntegral $ version header
sumVersionNumbers (Packet header (Operator packets)) =
    fromIntegral (version header) + sum (map sumVersionNumbers packets)

versions (Packet header (Literal _)) = [version header]
versions (Packet header (Operator packets)) = 
    version header : concatMap versions packets

eval (Packet _ (Literal x)) = x
eval (Packet header (Operator operands)) = evalFnForType (typeID header) (map eval operands)

evalFnForType 0 = sum
evalFnForType 1 = product
evalFnForType 2 = minimum
evalFnForType 3 = maximum
evalFnForType 5 = convertBoolOp (>)
evalFnForType 6 = convertBoolOp (<)
evalFnForType 7 = convertBoolOp (==)

convertBoolOp op [a, b] | op a b    = 1
                        | otherwise = 0

toByteString = fromJust . decodeHex . pack

parseHeader = do
    version <- BG.getAsWord8 3
    typeID <- BG.getAsWord8 3
    return $ Header { version = version, typeID = typeID }

parsePacket = do
    header <- parseHeader
    (body, bitCount) <- if typeID header == 4 
                        then parseLiteralPacket 
                        else parseOperatorPacket
    return (Packet header body, 6 + bitCount)

parseLiteralPacket :: BG.BitGet (PacketBody, Word16)
parseLiteralPacket = map1 Literal <$> varLengthNum

map1 f (a, b) = (f a, b)

varLengthNum = varLengthNum' 0 0
varLengthNum' :: Word64 -> Word16 -> BG.BitGet (Word64, Word16)
varLengthNum' acc count = do
    hasMore <- BG.getBit
    chunk <- BG.getAsWord64 4
    let nextNum = (acc `shiftL` 4) + chunk
    let nextCount = count + 5
    if hasMore 
    then varLengthNum' nextNum nextCount
    else return (nextNum, nextCount)

parseOperatorPacket = do
    lengthBit <- BG.getBit
    size <- BG.getAsWord16 $ if lengthBit then 11 else 15
    if lengthBit 
    then (map1 Operator . fmap (+12)) <$> parseFixedCountPackets size
    else ((,size + 16) . Operator) <$> parseBitCountedPackets size

parseBitCountedPackets :: Word16 -> BG.BitGet [Packet]
parseBitCountedPackets 0 = return []
parseBitCountedPackets bitsLeft = do
    (packet, bitCount) <- parsePacket
    (packet:) <$> parseBitCountedPackets (bitsLeft - bitCount)

parseFixedCountPackets 0 = return ([], 0)
parseFixedCountPackets size = do
    (packet, bitCount) <- parsePacket
    (packets, nextBitCount) <- parseFixedCountPackets (size - 1)
    return (packet:packets, bitCount + nextBitCount)

fromRight (Right x) = x
fromRight x = error $ show x

