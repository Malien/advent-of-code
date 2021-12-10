file = open("input.txt")
contents = file.read()
file.close()

def value(char):
    if char == '(':
        return 1
    elif char == '[':
        return 2
    elif char == '{':
        return 3
    elif char == '<':
        return 4

scrs = []
for line in contents.split("\n"):
    stack = []
    discard = False
    for char in line:
        if char == ')':
            if stack.pop() != '(':
                discard = True
                break
        elif char == ']':
            if stack.pop() != '[':
                discard = True
                break
        elif char == '}':
            if stack.pop() != '{':
                discard = True
                break
        elif char == '>':
            if stack.pop() != '<':
                discard = True
        else:
            stack.append(char)
    if discard:
        continue
    score = 0
    for left in reversed(stack):
        score *= 5
        score += value(left)
    if score != 0:
        scrs.append(score)

scrs.sort()
print(scrs)
print(scrs[len(scrs) // 2])
