file = open("input.txt")
contents = file.read().strip()
file.close()

initial = map(int, contents.split(","))

#  fish = list(map(int, contents.split(",")))

#  for day in range(256):
    #  for idx in range(len(fish)):
        #  if fish[idx] == 0:
            #  fish.append(8)
            #  fish[idx] = 6
        #  else:
            #  fish[idx] -= 1


fish = [0] * 7
waiting = [0,0]

for f in initial:
    fish[f] += 1

for day in range(256):
    to_hatch_today = fish[day % 7]
    fish[day % 7] += waiting[0]
    waiting[0] = waiting[1]
    waiting[1] = to_hatch_today

print(sum(fish) + sum(waiting))

