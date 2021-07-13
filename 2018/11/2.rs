// cargo-deps: rayon = "1.5.1"
extern crate rayon;
use std::fs;
use rayon::prelude::*;

fn power_level(x: i64, y: i64, serial_number: i64) -> i64 {
	let rack_id = x + 10;
	((rack_id * y + serial_number) * rack_id / 100) % 10 - 5
}

fn main() -> Result<(), ()> {
	let size = 300;
	let serial_number = fs::read_to_string("input.txt").map_err(|_| ())?.trim().parse().map_err(|_| ())?;

	let max = (1..=size)
		.into_par_iter()
		.map(|section_size| {
			let mut max = (0, 0, 0, 0);
			for y in 1..=(size - section_size) {
				for x in 1..=(size - section_size) {
					let mut section_power = 0;
					for dx in 0..section_size {
						for dy in 0..section_size {
							section_power += power_level(x + dx, y + dy, serial_number);
						}
					}
					if section_power > max.3 {
						max = (x, y, section_size, section_power);
					}
				}
			}
			max
		})
		.reduce(|| (0, 0, 0, 0), |a, c| if a.3 > c.3 { a } else { c });

	println!("{},{},{}", max.0, max.1, max.2);

	Ok(())
}
