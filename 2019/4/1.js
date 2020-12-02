function hasDouble(number) {
	return /(.)\1/.test(number.toString());
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