use std::fs::File;
use std::io::Read;

fn main() {
	let mut file = File::open("input.txt").unwrap();
	let mut input = String::new();
	file.read_to_string(&mut input).unwrap();
	let bytes = input.as_bytes();

	println!("Part 1: {}", part1(&bytes));
	println!("Part 2: {}", part2(&bytes));
}

fn parse(input: &[u8]) -> Vec<i32> {
	let mut crabs = Vec::with_capacity(1000);
	let mut current_crab = 0;
	let mut current_b;
	for i in 0..(input.len() - 1) {
		current_b = input[i] as i32 - 48;
		if current_b.is_negative() {
			crabs.push(current_crab);
			current_crab = 0;
		} else {
			current_crab = current_crab * 10 + current_b;
		}
	}
	crabs.push(current_crab);
	crabs
}

fn part1(input: &[u8]) -> i32 {
	let mut crabs = parse(input);

	let len = crabs.len();
	let target = *crabs.select_nth_unstable(len / 2).1;

	let mut value = 0;
	for i in 0..crabs.len() {
		value += (target - crabs[i]).abs();
	}
	value
}

fn part2(input: &[u8]) -> i32 {
	#[inline]
	fn trig(x: i32) -> i32 {
		x * (x + 1) / 2
	}

	let crabs = parse(input);

	let mut average = 0;
	for i in 0..crabs.len() {
		average += crabs[i];
	}
	average /= crabs.len() as i32;
	let mut value = 0;
	for i in 0..crabs.len() {
		value += trig((crabs[i] - average).abs());
	}
	value
}
