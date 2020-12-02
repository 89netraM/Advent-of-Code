const fs = require("fs");

function isValid(line) {
	let [_, low, high, c, password] = /(\d+)-(\d+) (.): (.+)/.exec(line);
	low = parseInt(low);
	high = parseInt(high);
	password = [...password];
	const actual = password.reduce((p, x) => x === c ? p + 1 : p, 0);
	return low <= actual && actual <= high ? 1 : 0;
}

const lines = fs.readFileSync("./input.txt").toString().split("\n");
console.log(lines.reduce((p, x) => p + isValid(x), 0));