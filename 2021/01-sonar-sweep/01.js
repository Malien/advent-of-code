import { promises as fs } from "node:fs"

const file = await readFile("input.txt")
console.log(file.toString().split("\n").map(v => parseInt(v))
