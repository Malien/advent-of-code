import { readFile } from 'fs/promises';

const splledDigits = [
  'zero', 'one', 'two', 'three', 'four',
  'five', 'six', 'seven', 'eight', 'nine'
];

const regex = /(one)|(two)|(three)|(four)|(five)|(six)|(seven)|(eight)|(nine)|([1-9])/g;

const file = await readFile('input.txt', 'utf8');
let sum = 0;
for (const line of file.split('\n').filter(Boolean)) {
    const nums = []
    for (const match of line.matchAll(regex)) {
        const digit = match.filter(v => v !== undefined)[0];
        if (digit === undefined) {
            continue;
        }
        const idx = splledDigits.indexOf(digit);
        if (idx === -1) {
            nums.push(+digit);
        } else {
            nums.push(idx);
        }
    }
    sum += nums[0] * 10 + nums.at(-1);
}
console.log(sum);
