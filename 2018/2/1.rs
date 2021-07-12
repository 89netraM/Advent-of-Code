use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::HashMap;

fn main() {
	let mut twos = 0;
	let mut threes = 0;
	let mut map = HashMap::new();
	for line in BufReader::new(File::open("input.txt").unwrap()).lines().map(|rl| rl.unwrap()) {
		for c in line.chars() {
			map.entry(c)
			.and_modify(|n| { *n += 1 })
			.or_insert(1);
		}
		let mut seenTwo = false;
		let mut seenThree = false;
		for (_, n) in map.drain() {
			if !seenTwo && n == 2 {
				twos += 1;
				seenTwo = true;
			} else if !seenThree && n == 3 {
				threes += 1;
				seenThree = true;
			}
		}
	}
	println!("{}", twos * threes);
}
