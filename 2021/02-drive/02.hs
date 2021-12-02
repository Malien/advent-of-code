main = readFile "input.txt" >>= print . (uncurry (*)) . foldr applyCommand (0,0) . map parseCommand . lines

data Comand = Command (Int, Int) Int

parseCommand str = Command (commandDirection command) (read amount)
    where [command, amount] = words str

commandDirection "forward" = (1, 0)
commandDirection "down" = (0, 1)
commandDirection "up" = (0, -1)

applyCommand (Command (dx, dy) amount) (x, y) = (x + dx * amount, y + dy * amount)

