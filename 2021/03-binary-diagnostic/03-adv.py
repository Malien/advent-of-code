file = open("input.txt", "rt")
contents = file.read()
file.close()

lines = contents.strip().split('\n')
counts = [0] * len(lines[0])
for line in lines:
    for idx, bit in enumerate(line):
        if bit == "1":
            counts[idx] += 1

oxygen = lines[:]
newoxy = []
co2 = lines[:]
newco2 = []

def count_bit(idx, lines):
    count = 0
    for line in lines:
        if line[idx] == '1':
            count += 1
    return count

def most_common(bit, count, total):
    if count >= total - count:
        return bit == '1'
    else:
        return bit == '0'

def least_common(bit, count, total):
    if count < total - count:
        return bit == '1'
    else:
        return bit == '0'

for idx, count in enumerate(counts):
    count = count_bit(idx, oxygen)
    oxygen = [
        line for line in oxygen 
        if most_common(line[idx], count, len(oxygen))
    ]
    if len(oxygen) == 1:
        print("oxygen", int(oxygen[0], 2))
        break

for idx in range(len(counts)):
    count = count_bit(idx, co2)
    co2 = [
        line for line in co2 
        if least_common(line[idx], count, len(co2))
    ]
    if len(co2) == 1:
        print("co2", int(co2[0], 2))
        break

