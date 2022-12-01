using System;
using System.Linq;
using AoC.Library;

namespace AoC.Year2022;

[Day(1)]
public class Day1
{
	[Part(1)]
	public object Part1(string input) =>
		input
			.Split("\n\n")
			.Select(e => e.Lines()
				.Select(Int64.Parse)
				.Sum())
			.Max();

	[Part(2)]
	public object Part2(string input) =>
		input
			.Split("\n\n")
			.Select(e => e.Lines()
				.Select(Int64.Parse)
				.Sum())
			.OrderDescending()
			.Take(3)
			.Sum();
}
