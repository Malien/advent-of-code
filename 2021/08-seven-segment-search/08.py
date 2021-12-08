from itertools import permutations

file = open("input.txt")
contents = file.read().strip()
file.close()

templ = [
    [0, 1, 2, 4, 5, 6],
    [2, 5],
    [0, 2, 3, 4, 6],
    [0, 2, 3, 5, 6],
    [1, 2, 3, 5],
    [0, 1, 3, 5, 6],
    [0, 1, 3, 4, 5, 6],
    [0, 2, 5],
    [0, 1, 2, 3, 4, 5, 6],
    [0, 1, 2, 3, 5, 6]
    ]
templ = [set(i) for i in templ]

def get_config(digits):
    digits = [set(i) for i in digits]
    one, four, seven, eight = None, None, None, None
    for number in digits:
        if len(number) == 2:
            one = number
        elif len(number) == 4:
            four = number
        elif len(number) == 3:
            seven = number
        elif len(number) == 7:
            eight = number

    for p in permutations("abcdefg"):
        p = list(p)
        sone = set([p[2], p[5]]) == one
        sfour = set([p[1], p[2], p[3], p[5]]) == four
        sseven = set([p[0], p[2], p[5]]) == seven
        if sone and sfour and sseven:
            r = []
            for num in digits:
                segments = set(p.index(d) for d in num)
                r.append(segments in templ)
            if all(r):
                return p

count = 0
for line in contents.split("\n"):
    digits, input = line.split(" | ")
    p = get_config(digits.split(" "))
    res = 0
    for num in input.split(" "):
        segments = set(p.index(d) for d in num)
        digit = templ.index(segments)
        res *= 10
        res += digit
    count += res

print(count)

