{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}

import           Data.Array        (Array, listArray, (!))
import           Data.Bits         (Bits (shiftR, xor, (.&.)))
import           Data.List         (intercalate)
import           Data.Word         (Word8)
import           Debug.Trace       (trace)
import           Text.RawString.QQ (r)
import           Text.Regex.PCRE   (AllTextMatches (getAllTextMatches), (=~))

main = readFile "in" >>= print . process

test = tail [r|
Register A: 117440
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0
|]

process = intercalate "," . map printable . reverse . output . execute . parseMachine

parseMachine input = Machine {
    regA = read a,
    regB = read b,
    regC = read c,
    pc = 0,
    program = listArray (0, length program - 1) $ map (parseValue . read) program,
    output = [],
    outputIdx = 0
  }
  where (a:b:c:program) = getAllTextMatches $ input =~ "(\\d+)"

newtype Value = Value Word8 deriving (Num, Eq, Ord, Integral, Real, Enum, Bits, Bounded, Show, Read)
data Operand = Literal Value | RegisterA | RegisterB | RegisterC
data Opcode = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV deriving (Bounded, Enum, Eq, Show)

printable :: Value -> String
printable (Value v) = show v

parseValue v | v `elem` [0..7] = Value v

parseOpcode :: Value -> Opcode
parseOpcode = ([minBound ..] !!) . fromIntegral

evalCombo :: Machine -> Value -> Int
evalCombo machine 0 = 0
evalCombo machine 1 = 1
evalCombo machine 2 = 2
evalCombo machine 3 = 3
evalCombo machine 4 = regA machine
evalCombo machine 5 = regB machine
evalCombo machine 6 = regC machine

data Machine = Machine {
  regA      :: Int,
  regB      :: Int,
  regC      :: Int,
  pc        :: Int,
  program   :: Array Int Value,
  output    :: [Value],
  outputIdx :: Int
} deriving (Eq, Show)

execute machine@(Machine { pc, program, output })
  | pc >= length program = machine
execute machine@(Machine { pc, program, output = (head:_), outputIdx })
  | (program ! (outputIdx - 1)) /= head = machine
execute machine@(Machine { pc, program }) = execute $ evalOp machine opcode operand
  where opcode = parseOpcode $ program ! pc
        operand = program ! (pc + 1)

advance machine = machine { pc = pc machine + 2 }

truncateToValue :: Int -> Value
truncateToValue = fromIntegral . (.&. 0b111)

evalOp' machine = evalOp (machineBits machine)

evalOp machine ADV operand = advance $ machine { regA = regA machine `shiftR` evalCombo machine operand }
evalOp machine BXL operand = advance $ machine { regB = regB machine `xor` fromIntegral operand }
evalOp machine BST operand = advance $ machine { regB = evalCombo machine operand .&. 0b111 }
evalOp m@(Machine { regA = 0 }) JNZ _ = advance m
evalOp machine JNZ operand = machine { pc = fromIntegral operand }
evalOp machine BXC _ignore = advance $ machine { regB = regB machine `xor` regC machine }
evalOp machine OUT operand = advance $ machine
  { output = truncateToValue (evalCombo machine operand) : output machine
  , outputIdx = outputIdx machine + 1
  }
evalOp machine BDV operand = advance $ machine { regB = regA machine `shiftR` evalCombo machine operand }
evalOp machine CDV operand = advance $ machine { regC = regA machine `shiftR` evalCombo machine operand }

blank program = Machine 0 0 0 0 (listArray (0, length program - 1) program) [] 0

machineBits machine = trace (
  "A: " ++ show (regA machine) ++
  "\tB: " ++ show (regB machine) ++
  "\tC: " ++ show (regC machine) ++
  "\tPC: " ++ show (pc machine) ++
  "\topcode: " ++ show (parseOpcode $ program machine ! pc machine) ++
  "\toperand: " ++ show (program machine ! (pc machine + 1)) ++
  "\tcombo: " ++ show (evalCombo machine (program machine ! (pc machine + 1))) ++
  "\toutput: " ++ show (reverse $ output machine)
  ) machine

-- If register C contains 9, the program 2,6 would set register B to 1.
case1 = then'
  where given = (blank [2,6]) { regC = 9 }
        when = execute given
        then' = regB when == 1

-- If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2.
case2 = then'
  where given = (blank [5,0,5,1,5,4]) { regA = 10 }
        when = execute given
        then' = output when == [0,1,2]

-- If register A contains 2024, the program 0,1,5,4,3,0 would output 4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.
case3 = then'
  where given = (blank [0,1,5,4,3,0]) { regA = 2024 }
        when = execute given
        then' = reverse  (output when) == [4,2,5,6,7,7,7,7,3,1,0] && regA when == 0

-- If register B contains 29, the program 1,7 would set register B to 26.
case4 = then'
  where given = (blank [1,7]) { regB = 29 }
        when = execute given
        then' = regB when == 26

-- If register B contains 2024 and register C contains 43690, the program 4,0 would set register B to 44354.
case5 = then'
  where given = (blank [4,0]) { regB = 2024, regC = 43690 }
        when = execute given
        then' = regB when == 44354

testCases = [case1, case2, case3, case4, case5]
