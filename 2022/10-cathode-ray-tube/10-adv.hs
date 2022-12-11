{-# LANGUAGE NamedFieldPuns #-}
import           Data.List.Split (chunksOf)
import           Prelude         hiding (cycle)

main = readFile "in" >>= putStr . process

process =
    screenDisplay
  . foldl evalInstr cpuInitialState
  . map parseInstr
  . lines

data Instr = AddX Int | Nop

parseInstr "noop" = Nop
parseInstr str    = AddX $ read $ drop 5 str

data CPU = CPU {
  state  :: Int,
  cycle  :: Int,
  screen :: String
}

cpuInitialState = CPU { state = 1, cycle = 0, screen = "" }

evalInstr (CPU { screen, state, cycle }) Nop = CPU
  { screen = screen ++ [pixel cycle state]
  , state
  , cycle = cycle + 1
  }
evalInstr (CPU { screen, state, cycle }) (AddX a) = CPU
  { screen = screen ++ [pixel cycle state, pixel (cycle + 1) state]
  , state = state + a
  , cycle = cycle + 2
  }

pixel cycle state | column `elem` [state - 1, state, state + 1] = '#'
                  | otherwise                                   = ' '
                  where column = cycle `mod` screenWidth

screenWidth = 40

screenDisplay = unlines . chunksOf screenWidth . screen
