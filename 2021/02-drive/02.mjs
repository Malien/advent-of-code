import { promises as fs } from "node:fs"

const parseCommand = str => {
    const [command, amount] = str.split(" ")
    return { command, amount: parseInt(amount) }
}

const applyCommand = ({ pos: [x, y], aim }, { command, amount }) => {
    switch (command) {
    case "forward":
        return { pos: [x + amount, y + aim * amount], aim }
    case "down":
        return { pos: [x, y], aim: aim + amount }
    case "up":
        return { pos: [x, y], aim: aim - amount }
    }
}

const file = await fs.readFile("input.txt")
const { pos: [x, y] } = file
    .toString()
    .trim()
    .split("\n")
    .map(parseCommand)
    .reduce(applyCommand, { pos: [0, 0], aim: 0 })

console.log(x * y)

