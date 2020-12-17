use std::fs::File;
use std::io::{ BufReader, Read, Result };
use std::collections::{ HashSet, HashMap };
use std::mem;
use std::time::Instant;

fn main() -> Result<()> {
	let parsing = Instant::now();
	let mut buf = BufReader::new(File::open("input.txt")?);
	let mut content = String::new();
	buf.read_to_string(&mut content)?;

	let mut set = HashSet::new();
	for (line, y) in content.lines().zip(0..) {
		for (c, x) in line.chars().zip(0..) {
			if c == '#' {
				set.insert(Coord::new(x, y, 0));
			}
		}
	}

	println!("Reading & parsing: {:#?}", parsing.elapsed());
	let doing = Instant::now();

	let mut next_set = HashSet::with_capacity(set.capacity());
	let mut map = HashMap::with_capacity(set.capacity());
	for _ in 0..6 {
		for coord in &set {
			for adjacent in coord.adjacent() {
				map.entry(adjacent).and_modify(|c| *c += 1).or_insert(1);
			}
		}

		for (coord, adjacent_count) in map.drain() {
			if adjacent_count == 3 || (set.contains(&coord) && adjacent_count == 2) {
				next_set.insert(coord);
			}
		}

		mem::swap(&mut set, &mut next_set);
		next_set.clear();
	}

	println!("Calculating:       {:#?}", doing.elapsed());
	println!("{}", set.len());

	Ok(())
}

//#region Coord
#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, Hash)]
struct Coord {
	pub x: isize,
	pub y: isize,
	pub z: isize,
}

impl Coord {
	pub fn new(x: isize, y: isize, z: isize) -> Coord {
		Coord { x, y, z }
	}

	pub fn adjacent(&self) -> AdjacentCoords {
		AdjacentCoords::new(self)
	}
}

struct AdjacentCoords<'a> {
	coord: &'a Coord,
	x: isize,
	y: isize,
	z: isize,
}

impl AdjacentCoords<'_> {
	pub fn new<'a>(coord: &'a Coord) -> AdjacentCoords<'a> {
		AdjacentCoords { coord, x: -1, y: -1, z: -2 }
	}

	fn increment(&'_ mut self) -> bool {
		self.z += 1;
		if self.z > 1 {
			self.z = -1;
			self.y += 1;
			if self.y > 1 {
				self.y = -1;
				self.x += 1;
				if self.x > 1 {
					return false;
				}
			}
		}

		if self.x == 0 && self.y == 0 && self.z == 0 {
			self.increment()
		}
		else {
			true
		}
	}
}

impl<'a> Iterator for AdjacentCoords<'a> {
	type Item = Coord;

	fn next(&mut self) -> Option<Self::Item> {
		if self.increment() {
			Some(Coord::new(
				self.coord.x + self.x,
				self.coord.y + self.y,
				self.coord.z + self.z
			))
		}
		else {
			None
		}
	}
}
//#endregion Coord
