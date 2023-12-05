import           Data.Char
import           Data.List       hiding (intersect)
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe
import           Data.Ord
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Debug.Trace
import           Text.Regex.PCRE

main = readFile "in" >>= print . process

test = "\
\seeds: 79 14 55 13\n\
\\n\
\seed-to-soil map:\n\
\50 98 2\n\
\52 50 48\n\
\\n\
\soil-to-fertilizer map:\n\
\0 15 37\n\
\37 52 2\n\
\39 0 15\n\
\\n\
\fertilizer-to-water map:\n\
\49 53 8\n\
\0 11 42\n\
\42 0 7\n\
\57 7 4\n\
\\n\
\water-to-light map:\n\
\88 18 7\n\
\18 25 70\n\
\\n\
\light-to-temperature map:\n\
\45 77 23\n\
\81 45 19\n\
\68 64 13\n\
\\n\
\temperature-to-humidity map:\n\
\0 69 1\n\
\1 0 69\n\
\\n\
\humidity-to-location map:\n\
\60 56 37\n\
\56 93 4"

process inp = minimum $ map start $ foldl transformOnce (parseSeeds seeds) (map parseMap maps)
    where (seeds:maps) = splitOn "\n\n" inp

pipeline seeds (mapping:mappings) = (seeds, "->", mapping) : pipeline (transformOnce seeds mapping) mappings
pipeline seeds [] = [(seeds, "end", [])]

data Mapping = Mapping { targetStart :: Int, sourceStart :: Int, mLen :: Int } deriving Show
data SeedRange = SeedRange { start :: Int, sLen :: Int } deriving Show

parseSeeds
    = map (\[start, sLen] -> SeedRange start sLen)
    . chunksOf 2
    . map read
    . words
    . drop (length "seeds: ")

parseMap = map (parseMapping . words) . tail . lines

parseMapping [a,b,c] = Mapping (read a) (read b) (read c)

-- cases to handle:
-- range: 1. [###]    2.    [###]
-- map  :      [###]      [###]
--
-- range: 3.  [###]   4. [#######]
-- map  :   [#######]      [###]

intersect s@(SeedRange { start, sLen }) (Mapping { sourceStart, mLen })
    | rangeStart <= mapStart && rangeEnd >= mapStart && rangeEnd <= mapEnd
    = (Just (SeedRange mapStart (rangeEnd - mapStart)), [
        SeedRange rangeStart (mapStart - rangeStart)
    ])
    | rangeStart >= mapStart && rangeStart <= mapEnd && rangeEnd >= mapEnd
    = (Just (SeedRange rangeStart (mapEnd - rangeStart)), [
        SeedRange mapEnd (rangeEnd - mapEnd)
    ])
    | rangeStart >= mapStart && rangeEnd <= mapEnd
    = (Just s, [])
    | rangeStart <= mapStart && rangeEnd >= mapEnd
    = (Just (SeedRange mapStart mLen), [
        SeedRange rangeStart (mapStart - rangeStart),
        SeedRange mapEnd (rangeEnd - mapEnd)
    ])
    | otherwise = (Nothing, [s])
    where (rangeStart, rangeEnd) = (start      , start + sLen      )
          (mapStart  , mapEnd  ) = (sourceStart, sourceStart + mLen)

filterOutEmpty (Just (SeedRange _ 0), notIntersected) = (Nothing, filter ((/= 0) . sLen) notIntersected)
filterOutEmpty (i, notIntersected) = (i, filter ((/= 0) . sLen) notIntersected)

transformRange range map@(Mapping { sourceStart, targetStart }) = case range `intersect` map of
    (Just (SeedRange { start, sLen }), notIntersected) ->
        (Just (SeedRange { start = start - sourceStart + targetStart, sLen }), notIntersected)
    (Nothing, notIntersected) -> (Nothing, notIntersected)

transformOnce ranges mappings = intersected ++ notIntersected
    where (intersected, notIntersected) = foldl accum ([], ranges) mappings

accum (intersected, notIntersected) mapping = (intersected ++ intersected', notIntersected')
    where (intersected', notIntersected') = foldl collect ([], []) transformed
          transformed = map (filterOutEmpty . (`transformRange` mapping)) notIntersected
collect (intersected, notIntersected) (Just i, n) = (i:intersected, notIntersected ++ n)
collect (intersected, notIntersected) (Nothing, n) = (intersected, notIntersected ++ n)

