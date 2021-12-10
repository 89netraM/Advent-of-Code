#![feature(option_result_unwrap_unchecked)]

use std::fs::File;
use std::io::Read;

fn main() {
	let mut file = File::open("input.txt").unwrap();
	let mut bytes = Vec::new();
	file.read_to_end(&mut bytes).unwrap();

	println!("Part 1: {}", unsafe { part1(&bytes) });
	println!("Part 2: {}", unsafe { part2(&bytes) });
}

unsafe fn part1(input: &[u8]) -> u64 {
	const SCORING: [u64; 8] = [0, 0, 3, 25137, 0, 57, 0, 1197];

	let mut score = 0;

	let mut stack = Vec::with_capacity(100);
	for i in 0..input.len() {
		let b = input[i];
		if b == b'\n' {
			stack.clear();
		} else {
			let bb = b & 0b11;
			if (bb >> 1) ^ (bb & 0b1) == 0 {
				stack.push(b);
			} else if (b & 0b1111000) != (stack.pop().unwrap_unchecked() & 0b1111000) {
				score += SCORING[(b >> 4) as usize];
			}
		}
	}

	score
}

unsafe fn part2(input: &[u8]) -> u64 {
	const SCORING: [u64; 8] = [0, 0, 1, 4, 0, 2, 0, 3];

	let mut scores = Vec::with_capacity(150);

	let mut stack = Vec::with_capacity(100);
	let mut skip = false;
	for i in 0..input.len() {
		let b = input[i];
		if b == b'\n' {
			if !skip {
				let mut local_score = 0;
				let mut j = stack.len() - 1;
				loop {
					local_score *= 5;
					local_score += SCORING[(stack[j] >> 4) as usize];
					if j == 0 {
						break;
					}
					j -= 1;
				}
				scores.push(local_score);
			}
			skip = false;
			stack.clear();
		} else {
			let bb = b & 0b11;
			if (bb >> 1) ^ (bb & 0b1) == 0 {
				stack.push(b);
			} else if (b & 0b1111000) != (stack.pop().unwrap_unchecked() & 0b1111000) {
				skip = true;
			}
		}
	}

	let len = scores.len();
	*scores.select_nth_unstable(len / 2).1
}
