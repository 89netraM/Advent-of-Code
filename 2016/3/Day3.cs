using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2016;

[Day(3)]
public class Day3
{
	[Part(1)]
	public int Part1(string input)
	{
		int possible = 0;
		int a = 0, b = 0, c = 0;

		for (int i = 0; i < input.Length; i++)
		{
			for (; input[i] == ' '; i++);
			for (; input[i] != ' '; i++)
			{
				a = a * 10 + (input[i] - '0');
			}
			for (; input[i] == ' '; i++);
			for (; input[i] != ' '; i++)
			{
				b = b * 10 + (input[i] - '0');
			}
			for (; input[i] == ' '; i++);
			for (; i < input.Length && input[i] != '\n'; i++)
			{
				c = c * 10 + (input[i] - '0');
			}

			possible += BoolToInt(a + b > c && b + c > a && c + a > b);

			a = 0;
			b = 0;
			c = 0;
		}

		return possible;
	}

	private static unsafe int BoolToInt(bool b) =>
		*(byte*)&b;

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
