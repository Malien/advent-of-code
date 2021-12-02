import { promises as fs } from "node:fs"

const file = await fs.readFile("input.txt")

let x = 0
let y = 0
let aim = 0
for (const line of file.toString().trim().split("\n")) {
    const [command, amount] = line.split(" ")
    switch (command) {
    case "up":
        aim -= parseInt(amount)
        continue
    case "down":
        aim += parseInt(amount)
        continue
    case "forward":
        x += parseInt(amount)
        y += aim * amount
        continue
    }
}

console.log(x * y)

