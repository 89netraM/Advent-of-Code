use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
	let mut previous: Vec<String> = Vec::new();
	for line in BufReader::new(File::open("input.txt").unwrap()).lines().map(|rl| rl.unwrap()) {
		for prev in &previous {
			let mut diff = 0;
			let mut diff_count = 0;
			for (i, (a, b)) in line.chars().zip(prev.chars()).enumerate() {
				if a != b {
					diff = i;
					diff_count += 1;
				}
			}
			if diff_count == 1 {
				println!("{}{}", &line[..diff], &line[diff + 1..]);
				return;
			}
		}
		previous.push(line);
	}
}
