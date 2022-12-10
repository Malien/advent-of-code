main = readFile "in" >>= print . process

data Instr = AddX Int | Nop

parseInstr "noop" = Nop
parseInstr str    = AddX $ read $ drop 5 str

process = 
    sum 
  . map (uncurry (*)) 
  . every40thCycle
  . drop 19
  . zip [1..]
  . reverse 
  . foldl produceCPUStates [1]
  . map parseInstr 
  . lines

every40thCycle [] = []
every40thCycle states@(state:_) = state : every40thCycle (drop 40 states)

produceCPUStates list@(prevState:_) instr = evalInstr prevState instr ++ list

evalInstr state Nop      = [state]
evalInstr state (AddX a) = [state + a, state]
