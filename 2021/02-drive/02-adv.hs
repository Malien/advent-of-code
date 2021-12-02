main = readFile "input.txt" >>= print . solution . lines

data Command = Up Int | Down Int | Forward Int
data State = State (Int, Int) Int

solution = finalRes . foldl applyCommand (State (0,0) 0) . map parseCommand 

parseCommand str = commandDirection command (read amount) 
    where [command, amount] = words str

commandDirection "forward" = Forward
commandDirection "down" = Down
commandDirection "up" = Up

applyCommand (State pos aim) (Up amount) = State pos (aim - amount)
applyCommand (State pos aim) (Down amount) = State pos (aim + amount)
applyCommand (State (x, y) aim) (Forward amount) = State (x + amount, y + aim * amount) aim

finalRes (State (x, y) _) = x * y










