// Collect stars by helping Santa solve puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
// Here's an easy puzzle to warm you up.
// Santa is trying to deliver presents in a large apartment building, but he can't find the right floor - the directions he got are a little confusing. He starts on the ground floor (floor 0) and then follows the instructions one character at a time.
// An opening parenthesis, (, means he should go up one floor, and a closing parenthesis, ), means he should go down one floor.
// The apartment building is very tall, and the basement is very deep; he will never find the top or bottom floors.
// For example:
// (()) and ()() both result in floor 0. 
// ((( and (()(()( both result in floor 3. 
// ))((((( also results in floor 3. 
// ()) and ))( both result in floor -1 (the first basement level). 
// ))) and )())()) both result in floor -3. 
// To what floor do the instructions take Santa?
// Your puzzle answer was 280.
// --- Part Two ---
// Now, given the same instructions, find the position of the first character that causes him to enter the basement (floor -1). The first character in the instructions has position 1, the second character has position 2, and so on.
// For example:
// ) causes him to enter the basement at character position 1. 
// ()()) causes him to enter the basement at character position 5. 
// What is the position of the character that causes Santa to first enter the basement?
// Your puzzle answer was 1797.
// Both parts of this puzzle are complete! They provide two gold stars: **
using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;

namespace AoC.Year2015
{
	[Day(1)]
	public class Day1
	{
		[Part(1)]
		public object Part1(string input) =>
			input.Select(static c => c == '(' ? 1 : -1).Sum();

		[Part(2)]
		public object Part2(string input)
		{
			long floor = 0;
			long position = 0;
			foreach (char c in input)
			{
				position++;
				floor += c == '(' ? 1 : -1;
				if (floor == -1)
					return position;
			}
			return -1;
		}
	}
}
