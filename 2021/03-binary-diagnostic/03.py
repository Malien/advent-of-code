file = open("input.txt", "rt")
contents = file.read()
file.close()

lines = contents.strip().split('\n')
counts = [0] * len(lines[0])
for line in lines:
    for idx, bit in enumerate(line):
        if bit == "1":
            counts[idx] += 1

gamma = 0
epsilon = 0
for count in counts:
    gamma = gamma << 1
    epsilon = epsilon << 1
    if count < len(lines) - count:
        epsilon += 1
    else:
        gamma += 1

print(gamma * epsilon)

