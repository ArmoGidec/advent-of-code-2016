const File = require("fs");


const lines = File.readFileSync("day10/input.txt", "utf-8").trim().split("\n");

const bots = [];
const values = [];
const output = [];
for (let line of lines) {
  const bot_rex = /^bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)$/;
  const val_rex = /^value (\d+) goes to (bot|output) (\d+)$/;

  let match;
  if ((match = bot_rex.exec(line))) {
    bots[+match[1]] = {low: [match[2], +match[3]], high: [match[4], +match[5]]};
  } else if ((match = val_rex.exec(line))) {
    if (!values[+match[3]]) values[+match[3]] = [];
    values[+match[3]].push(+match[1]);
  }
}

let special_bin = -1;
let stop = false;
while (!stop) {
  stop = true;
  for (let i = 0; i < values.length; i += 1) {
    if (!values[i] || values[i].length < 2) continue;

    stop = false;

    values[i].sort((x, y) => {
      // DARNIT
      if (x < y) return -1;
      if (x > y) return 1;
      return 0;
    });

    if (values[i][0] === 17 && values[i][1] === 61) {
      special_bin = i;
    }

    if (bots[i].low[0] !== "output") {
      if (!values[bots[i].low[1]]) values[bots[i].low[1]] = [];
      values[bots[i].low[1]].push(values[i][0]);
    } else {
      if (!output[bots[i].low[1]]) output[bots[i].low[1]] = [];
      output[bots[i].low[1]].push(values[i][0]);
    }

    if (bots[i].high[0] !== "output") {
      if (!values[bots[i].high[1]]) values[bots[i].high[1]] = [];
      values[bots[i].high[1]].push(values[i][1]);
    } else {
      if (!output[bots[i].high[1]]) output[bots[i].high[1]] = [];
      output[bots[i].high[1]].push(values[i][1]);
    }

    values[i] = [];
  }
}

console.log("Part One: " + special_bin);
console.log("Part Two: " + output[0][0] * output[1][0] * output[2][0]);