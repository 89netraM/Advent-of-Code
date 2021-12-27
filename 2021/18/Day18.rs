// cargo-deps: rayon="1.5.1"

extern crate rayon;
use std::fs::File;
use std::io::Read;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

const MATH_LEN: usize = 16;
const MATH_COUNT: usize = 100;

fn main() {
	let mut file = File::open("input.txt").unwrap();
	let mut bytes = Vec::new();
	file.read_to_end(&mut bytes).unwrap();

	println!("Part 1: {}", part1(&bytes));
	println!("Part 2: {}", part2(&bytes));
}

#[derive(Copy, Clone)]
struct Node {
	level: isize,
	value: isize,
}

impl Node {
	fn new(level: isize, value: isize) -> Self {
		Self { level, value }
	}
}

fn part1(input: &[u8]) -> isize {
	let mut math = Vec::with_capacity(MATH_LEN * 2);
	let mut i = 0;
	let mut level = 0;
	i += parse_line(input, i, level, &mut math) + 1;
	let len = input.len();
	while i < len {
		i += parse_line(input, i, level, &mut math) + 1;
		explode(&mut math, level);
		reduce(&mut math, level);
		level -= 1;
	}

	magnitude(&math)
}

fn part2(input: &[u8]) -> isize {
	let mut maths = Vec::with_capacity(MATH_COUNT);

	let mut i = 0;
	let len = input.len();
	while i < len {
		let mut math = Vec::with_capacity(MATH_LEN);
		i += parse_line(input, i, 0, &mut math) + 1;
		maths.push(math);
	}

	let len = maths.len();
	(0..len)
		.into_par_iter()
		.map(|i| {
			let mut math = Vec::with_capacity(MATH_LEN * 2);
			let mut max_mag = 0;
			let mut j = 0;
			while j < len {
				if i != j {
					math.clear();
					math.extend_from_slice(&maths[i]);
					math.extend_from_slice(&maths[j]);
					explode(&mut math, 0);
					reduce(&mut math, 0);
					max_mag = max_mag.max(magnitude(&math));
				}
				j += 1;
			}
			max_mag
		})
		.max()
		.unwrap()
}

fn parse_line(input: &[u8], offset: usize, mut level: isize, math: &mut Vec<Node>) -> usize {
	level += 1;
	let mut i = offset;
	loop {
		match input[i] {
			b'[' => level += 1,
			b',' => {}
			b']' => level -= 1,
			b'\n' => return i - offset,
			d => math.push(Node::new(level, (d - b'0') as isize)),
		}
		i += 1;
	}
}

fn explode(math: &mut Vec<Node>, base_level: isize) {
	let mut i = 0;
	let mut len = math.len();
	while i < len {
		let n = math[i];
		if (n.level - base_level) == 5 {
			if i > 0 {
				math[i - 1].value += n.value;
			}
			if i + 2 < len {
				math[i + 2].value += math[i + 1].value;
			}
			math.remove(i);
			math[i] = Node::new(n.level - 1, 0);
			len -= 1;
		}
		i += 1;
	}
}

fn reduce(math: &mut Vec<Node>, base_level: isize) {
	let mut i = 0;
	let mut len = math.len();
	while i < len {
		let n = math[i];
		if n.value > 9 {
			let lower = n.value / 2;
			let upper = n.value - lower;
			if n.level - base_level == 4 {
				if i > 0 {
					math[i - 1].value += lower;
				}
				math[i].value = 0;
				if i + 1 < len {
					math[i + 1].value += upper;
				}
				if i > 0 {
					i -= 1;
				} else {
					i += 1;
				}
			} else {
				math[i] = Node::new(n.level + 1, lower);
				math.insert(i + 1, Node::new(n.level + 1, upper));
				len += 1;
				if lower < 10 {
					i += 1;
				}
			}
		} else {
			i += 1;
		}
	}
}

fn magnitude(math: &[Node]) -> isize {
	let mut stack = Vec::new();
	let len = math.len();
	let mut i = 0;
	while i < len {
		let n = math[i];
		stack.push(n);
		let mut len = stack.len();
		while len > 1 {
			let left = stack[len - 2];
			let right = stack[len - 1];
			if left.level == right.level {
				stack[len - 2] = Node::new(left.level - 1, left.value * 3 + 2 * right.value);
				len -= 1;
				stack.pop();
			} else {
				break;
			}
		}
		i += 1;
	}
	stack[0].value
}
