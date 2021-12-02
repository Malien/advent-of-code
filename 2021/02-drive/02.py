file = open("input.txt")
contents = file.read().strip()
file.close()

x = 0
y = 0
aim = 0
for line in contents.split("\n"):
    command, amount = line.split(" ")
    if command == "forward":
        x += int(amount)
        y += aim * int(amount)
    elif command == "down":
        aim += int(amount)
    elif command == "up":
        aim -= int(amount)

print(x * y)
