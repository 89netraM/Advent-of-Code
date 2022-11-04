using System;
using AoC.Library;

namespace AoC.Year2016;

[Day(19)]
public class Day19
{
	[Part(1)]
	public object Part1(string input)
	{
		return Winner(Int64.Parse(input)) + 1;

		static long Winner(long count) =>
			count switch
			{
				1 => 0,
				2 => 0,
				3 => 2,
				_ => MathM.Mod((Winner((count >> 1) + (count & 1)) - (count & 1)) * 2, count),
			};
	}

	[Part(2)]
	public object Part2(string input)
	{
		long count = Int64.Parse(input);
		long winner = 0;
		for (long i = 2; i <= count; i++)
		{
			long removed = i >> 1;
			if (winner + 1 >= removed)
			{
				winner = MathM.Mod(winner + 2, i);
			}
			else
			{
				winner = MathM.Mod(winner + 1, i);
			}
		}
		return winner + 1;
	}
}
