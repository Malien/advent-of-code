{-# LANGUAGE NamedFieldPuns #-}
import           Data.Array
import           Data.List       (sortBy, sortOn)
import           Data.List.Split (splitOn)
import           Data.Ord        (Down (Down), comparing)
import           Prelude         hiding (round)
import           Text.Regex.PCRE (AllTextMatches (getAllTextMatches), (=~))

main = readFile "in" >>= print . process

process =
  product
  . take 2
  . sortOn Down
  . map inspected
  . elems
  . (!!10_000)
  . monkeyRounds
  . boundlessArray
  . parseMonkeys
  . splitOn "\n\n"

data MonkeyOp = Add Int | Mul Int | Square deriving Show

type Item = Array Int Int

data Monkey = Monkey {
  idx          :: Int,
  items        :: [Item],
  operation    :: MonkeyOp,
  divisionTest :: Int,
  trueMonkey   :: Int,
  falseMonkey  :: Int,
  inspected    :: Int
  } deriving Show

parseMonkeys monkeyDefs = map (parseMonkey $ length monkeyDefs) monkeyDefs

parseMonkey monkeyCount def = Monkey
  { idx = read (idDef =~ "\\d+")
  , items = map (initItem monkeyCount . read) $ getAllTextMatches (itemsDef =~ "\\d+")
  , operation = parseOperation operationDef
  , divisionTest = read (testDef =~ "\\d+")
  , trueMonkey = read $ drop 29 trueDef
  , falseMonkey = read $ drop 30 falseDef
  , inspected = 0
  }
  where [idDef, itemsDef, operationDef, testDef, trueDef, falseDef] = lines def

initItem monkeyCount num = listArray (0, monkeyCount - 1) $ repeat num

parseOperation "  Operation: new = old * old" = Square
parseOperation def = case def !! 23 of
  '*' -> Mul $ read $ drop 25 def
  '+' -> Add $ read $ drop 25 def

boundlessArray list = listArray (0, length list - 1) list

evalMonkey monkey monkeys = map (decideItem monkeys monkey) $ items monkey

evalOp (Add x) item = item + x
evalOp (Mul x) item = item * x
evalOp Square  item = item * item

decideItem monkeys monkey item = (distribute monkey res, res)
  where res = lowerItem monkeys $ fmap (evalOp (operation monkey)) item

lowerItem monkeys item = boundlessArray $ zipWith mod (elems item) (map divisionTest (elems monkeys))

distribute (Monkey { idx, divisionTest, trueMonkey }) item | (item ! idx) `mod` divisionTest == 0 = trueMonkey
distribute (Monkey { falseMonkey }) _ = falseMonkey

round monkeys = foldl update monkeys $ indices monkeys

update monkeys idx = accum throwItemAt afterProcessed updates
  where currentMonkey@Monkey { inspected, items } = monkeys ! idx
        updates = evalMonkey currentMonkey monkeys
        afterProcessed = monkeys // [(idx, currentMonkey {
          items = [],
          inspected = inspected + fromIntegral (length items)
        })]

throwItemAt monkey item = monkey { items = items monkey ++ [item] }

monkeyRounds monkeys = monkeys : monkeyRounds (round monkeys)

test =
  "Monkey 0:\n\
  \  Starting items: 79, 98\n\
  \  Operation: new = old * 19\n\
  \  Test: divisible by 23\n\
  \    If true: throw to monkey 2\n\
  \    If false: throw to monkey 3\n\
  \\n\
  \Monkey 1:\n\
  \  Starting items: 54, 65, 75, 74\n\
  \  Operation: new = old + 6\n\
  \  Test: divisible by 19\n\
  \    If true: throw to monkey 2\n\
  \    If false: throw to monkey 0\n\
  \\n\
  \Monkey 2:\n\
  \  Starting items: 79, 60, 97\n\
  \  Operation: new = old * old\n\
  \  Test: divisible by 13\n\
  \    If true: throw to monkey 1\n\
  \    If false: throw to monkey 3\n\
  \\n\
  \Monkey 3:\n\
  \  Starting items: 74\n\
  \  Operation: new = old + 3\n\
  \  Test: divisible by 17\n\
  \    If true: throw to monkey 0\n\
  \    If false: throw to monkey 1"
