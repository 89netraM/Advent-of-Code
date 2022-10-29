#![feature(test)]

extern crate test;
use test::Bencher;

const INPUT_FILE: &'static [u8] = include_bytes!("./input.txt");

fn main() {
	let input = &INPUT_FILE[..(INPUT_FILE.len() - 1)];
	let result = part1(input);
	println!("{}", result);
}

fn part1(input: &[u8]) -> usize {
	let mut possible = 0;
	let mut a: usize = 0;
	let mut b: usize = 0;
	let mut c: usize = 0;

	let mut i = 0;
	while i < input.len() {
		while input[i] == b' ' { i += 1; }
		while input[i] != b' ' {
			a = a * 10 + (input[i] - b'0') as usize;
			i += 1;
		}
		while input[i] == b' ' { i += 1; }
		while input[i] != b' ' {
			b = b * 10 + (input[i] - b'0') as usize;
			i += 1;
		}
		while input[i] == b' ' { i += 1; }
		while i < input.len() && input[i] != b'\n' {
			c = c * 10 + (input[i] - b'0') as usize;
			i += 1;
		}

		possible += (a + b > c && b + c > a && c + a > b) as usize;

		a = 0;
		b = 0;
		c = 0;
		i += 1;
	}

	possible
}

#[bench]
fn part1_bench(b: &mut Bencher) {
	let input = &INPUT_FILE[..(INPUT_FILE.len() - 1)];
	b.iter(|| part1(&input));
}
