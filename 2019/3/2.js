const fs = require("fs");

function createPath(line) {
	const path = new Object();
	let previous = [0, 0];
	let count = 1;

	for (const ins of line.split(",")) {
		let [, dir, length] = /([UDLR])(\d+)/.exec(ins);
		length = parseInt(length);
		const dirFn = {
			"U": ([x, y]) => [x, y + 1],
			"D": ([x, y]) => [x, y - 1],
			"L": ([x, y]) => [x - 1, y],
			"R": ([x, y]) => [x + 1, y]
		}[dir];
		for (let i = 0; i < length; i++) {
			previous = dirFn(previous);
			path[previous[0] + "," + previous[1]] = count++;
		}
	}

	return path;
}

const [wire1, wire2] = fs.readFileSync("./input.txt")
	.toString()
	.split("\n")
	.map(createPath);

let shortest = Number.POSITIVE_INFINITY;
for (const pos in wire1) {
	if (pos in wire2) {
		const distance = wire1[pos] + wire2[pos];
		if (distance < shortest) {
			shortest = distance;
		}
	}
}
console.log(shortest);