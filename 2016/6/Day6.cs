using System;
using System.Linq;
using AoC.Library;

namespace AoC.Year2016;

[Day(6)]
public class Day6
{
	[Part(1)]
	public object Part1(string input) =>
		input.Lines()
			.Transpose()
			.Select(p => p.ToCounter()
				.OrderByDescending(g => g.Value)
				.First())
			.Select(g => g.Key)
			.Let(String.Concat);

	[Part(2)]
	public object Part2(string input) =>
		input.Lines()
			.Transpose()
			.Select(p => p.ToCounter()
				.OrderBy(g => g.Value)
				.First())
			.Select(g => g.Key)
			.Let(String.Concat);
}
