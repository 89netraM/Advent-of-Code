function hasDouble(number) {
	const string = number.toString();
	const regex = /(.)\1(\1+)?/g;
	for (let match = regex.exec(string); match != null; match = regex.exec(string)) {
		if (match[2] == null) {
			return true;
		}
	}
	return false;
}

function isIncreasing(number) {
	const array = number.toString().split("").map(n => parseInt(n));
	for (let i = 1; i < array.length; i++) {
		if (array[i - 1] > array[i]) {
			return false;
		}
	}
	return true;
}

const input = [245182, 790572];

let count = 0;
for (let i = input[0]; i <= input[1]; i++) {
	if (hasDouble(i) && isIncreasing(i)) {
		count++;
	}
}
console.log(count);