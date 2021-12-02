import { promises as fs } from "node:fs"

const file = await fs.readFile("input.txt")

const required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

const between = (value, from, to) = value >= from && value <= to

function validField([key, value]) {
    switch (key) {
    case "byr":
        return value.length === 4 && between(parseInt(value), 1920, 2002)
    case "iyr":
        return value.length === 4 && between(parseInt(value), 2010, 2020)
    case "eyr":
        return value.length === 4 && between(parseInt(value), 2020, 2030)
    case "hgt":
        const clipped = value.substr(0, value.length-2)
        if (value.endsWith("cm")) {
            return between(parseInt(clipped), 150, 193))
        } 
        return value.endsWith("in") && between(parseInt(clipped), 59, 76)
    case "hcl":
        return value.startsWith("#") && 
    }
}

for (const passport of file.split("\n\n")) {
    const fields = passport.split(/ |\n/).map(str => str.split(":"))
    const hasRequired = required.every(([key, _] => required.includes(key))
    const validFields
}
