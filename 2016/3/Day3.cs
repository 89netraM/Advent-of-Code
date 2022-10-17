using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2016;

[Day(3)]
public class Day3
{
	[Part(1)]
	public object Part1(string input)
	{
		long possible = 0;
		foreach (var (a, b, c) in input.Lines().Extract<(long, long, long)>(@"(\d+)\s+(\d+)\s+(\d+)"))
		{
			if (a + b > c && b + c > a && c + a > b)
			{
				possible++;
			}
		}
		return possible;
	}

	[Part(2)]
	public object Part2(string input)
	{
		long possible = 0;
		var tris = new List<(long, long, long)>();
		var cols = input.Lines().Extract<(long, long, long)>(@"(\d+)\s+(\d+)\s+(\d+)").ToArray();
		for (int i = 0; i < cols.Length; i += 3)
		{
			tris.Add((cols[i + 0].Item1, cols[i + 1].Item1, cols[i + 2].Item1));
			tris.Add((cols[i + 0].Item2, cols[i + 1].Item2, cols[i + 2].Item2));
			tris.Add((cols[i + 0].Item3, cols[i + 1].Item3, cols[i + 2].Item3));
		}
		foreach (var (a, b, c) in tris)
		{
			if (a + b > c && b + c > a && c + a > b)
			{
				possible++;
			}
		}
		return possible;
	}
}
