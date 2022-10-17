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
		foreach (var chunk in input.Lines().Extract<(long, long, long)>(@"(\d+)\s+(\d+)\s+(\d+)").Chunk(3))
		{
			if (chunk[0].Item1 + chunk[1].Item1 > chunk[2].Item1 && chunk[1].Item1 + chunk[2].Item1 > chunk[0].Item1 && chunk[2].Item1 + chunk[0].Item1 > chunk[1].Item1)
			{
				possible++;
			}
			if (chunk[0].Item2 + chunk[1].Item2 > chunk[2].Item2 && chunk[1].Item2 + chunk[2].Item2 > chunk[0].Item2 && chunk[2].Item2 + chunk[0].Item2 > chunk[1].Item2)
			{
				possible++;
			}
			if (chunk[0].Item3 + chunk[1].Item3 > chunk[2].Item3 && chunk[1].Item3 + chunk[2].Item3 > chunk[0].Item3 && chunk[2].Item3 + chunk[0].Item3 > chunk[1].Item3)
			{
				possible++;
			}
		}
		return possible;
	}
}
