import           Data.List
import           Text.Regex.PCRE
import           Control.Monad
import           Control.Monad.ST
import           Data.Array.MArray
import           Data.Array.ST
import           Data.Array.Unboxed

main = process <$> readFile "in" >>= print

data Action = TurnOn | TurnOff | Toggle deriving Show

test = "turn on 0,0 through 999,999\ntoggle 0,0 through 999,0\nturn off 499,499 through 500,500"

action line | isPrefixOf "turn on" line = TurnOn
action line | isPrefixOf "turn off" line = TurnOff
action line | isPrefixOf "toggle" line = Toggle

parse :: String -> (Action, (Int, Int), (Int, Int))
parse line = (action line, (xa, xb), (ya, yb))
  where [xa, xb, ya, yb] = map read $ getAllTextMatches (line =~ "\\d+")

indecies (ax, ay) (bx, by) = [(x, y) | x <- [ax .. bx], y <- [ay .. by]]

applySingle TurnOn _ = True
applySingle TurnOff _ = False
applySingle Toggle x = not x

apply arr (action, start, end) =
  forM_ (indecies start end) (\index -> do
    brightness <- readArray arr index
    writeArray arr index $ applySingle action brightness
    )

baseArr :: ST s (STUArray s (Int, Int) Bool)
baseArr = newArray ((0, 0), (999, 999)) False

processArr actions = runSTUArray $ do
  arr <- baseArr
  forM_ actions (apply arr)
  return arr

process = length . filter id . elems . processArr . map parse . lines
