import { promises as fs } from "node:fs"

const file = await fs.readFile("input.txt")
const numbers = file.toString().split('\n').map((v) => parseInt(v))

function problem(report) {
    const set = new Set(report)
    for (const a of report) {
        if (set.has(2020 - a)) {
            return a * (2020 - a)
        }
    }
}

const start = performance.now()
const res = problem(numbers)
const end = performance.now()

console.log(`Took ${end - start}ms`)

