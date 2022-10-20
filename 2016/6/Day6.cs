using System;
using System.Linq;
using AoC.Library;

namespace AoC.Year2016;

[Day(6)]
public class Day6
{
	[Part(1)]
	public object Part1(string input)
	{
		var pos = new string[8];
		foreach (var l in input.Lines())
		{
			for (int i = 0; i < 8; i++)
			{
				pos[i] = (pos[i] ?? "") + l[i];
			}
		}
		return String.Concat(
			pos.Select(p => p.ToCounter()
					.OrderByDescending(g => g.Value)
					.First())
				.Select(g => g.Key));
	}

	[Part(2)]
	public object Part2(string input)
	{
		var pos = new string[8];
		foreach (var l in input.Lines())
		{
			for (int i = 0; i < 8; i++)
			{
				pos[i] = (pos[i] ?? "") + l[i];
			}
		}
		return String.Concat(
			pos.Select(p => p.ToCounter()
					.OrderBy(g => g.Value)
					.First())
				.Select(g => g.Key));
	}
}
