use std::fs::File;
use std::hint::unreachable_unchecked;
use std::io::Read;

fn main() {
	let mut file = File::open("input.txt").unwrap();
	let mut bytes = Vec::new();
	file.read_to_end(&mut bytes).unwrap();
	println!("{}", part1(&bytes));
	println!("{}", unsafe { part2(&bytes) });
}

fn part1(input: &[u8]) -> i32 {
	let mut count = 0;

	let mut in_output = false;
	let mut length = 0;
	for i in 0..input.len() {
		let b = input[i];
		if in_output {
			if b < b'a' {
				if (2 <= length && length <= 4) || length == 7 {
					count += 1;
				}
				if b == b'\n' {
					in_output = false;
				}
				length = 0;
			} else {
				length += 1;
			}
		} else if b == b'|' {
			in_output = true;
		}
	}

	count
}

unsafe fn part2(input: &[u8]) -> i32 {
	let mut sum = 0;

	let mut in_output = false;
	let mut all2 = Set(0);
	let mut all4 = Set(0);
	let mut output = Vec::new();
	let mut start = 0;
	let mut i = 0;
	let len = input.len();
	while i < len {
		let b = *input.get_unchecked(i);
		if b == b'|' {
			in_output = true;
			start = i + 2;
			i += 1;
		} else if b < b'a' {
			let set = Set::new(input.get_unchecked(start..i));
			if in_output {
				output.push(set);
			} else {
				match set.len() {
					2 => all2 = set,
					4 => all4 = set,
					_ => (),
				};
			}

			if b == b'\n' {
				let mut n = 0;
				for o in output.drain(..) {
					n = n * 10
						+ match (o.len(), all4.c_len(&o), all2.c_len(&o)) {
							(2, _, _) => 1,
							(3, _, _) => 7,
							(4, _, _) => 4,
							(7, _, _) => 8,
							(5, 2, _) => 2,
							(5, 3, 1) => 5,
							(5, 3, 2) => 3,
							(6, 4, _) => 9,
							(6, 3, 1) => 6,
							(6, 3, 2) => 0,
							_ => unreachable_unchecked(),
						};
				}
				sum += n;
				in_output = false;
			}
			start = i + 1;
		}
		i += 1;
	}

	sum
}

#[derive(Clone, Copy)]
struct Set(u8);

impl Set {
	fn new(s: &[u8]) -> Self {
		let mut seg = 0;
		for b in s {
			seg |= 1 << (*b - b'a');
		}
		Set(seg)
	}

	#[inline]
	fn len(&self) -> u32 {
		self.0.count_ones()
	}

	#[inline]
	fn c_len(&self, other: &Self) -> u32 {
		(self.0 & other.0).count_ones()
	}
}
