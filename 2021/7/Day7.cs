using System;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;

namespace AoC.Year2021
{
	[Day(7)]
	public class Day7
	{
		[Part(1)]
		public object Part1(string input)
		{
			var crabs = input.Split(',').Select(long.Parse).OrderBy(Id).ToArray();
			long target;
			if (crabs.Length % 2 == 0)
			{
				var mid = crabs.Length / 2;
				target = (crabs[mid - 1] + crabs[mid]) / 2;
			}
			else
			{
				var mid = (crabs.Length - 1) / 2;
				target = crabs[mid];
			}
			return crabs.Sum(c => Math.Abs(c - target));
		}

		[Part(2)]
		public object Part2(string input)
		{
			static long Sum(long x) => x * (x + 1) / 2;

			var crabs = input.Split(',').Select(long.Parse).ToArray();
			var min = (int)crabs.Min();
			var max = (int)crabs.Max();
			long [,] dp = new long[crabs.Length, max - min + 1];
			for (int i = 0; i < crabs.Length; i++)
			{
				for (int j = min; j <= max; j++)
				{
					if (i == 0)
					{
						dp[i, j - min] = Sum(Math.Abs(crabs[i] - j));
					}
					else
					{
						dp[i, j - min] = dp[i - 1, j - min] + Sum(Math.Abs(crabs[i] - j));
					}
				}
			}
			var minimum = long.MaxValue;
			for (int j = min; j <= max; j++)
			{
				minimum = Math.Min(minimum, dp[crabs.Length - 1, j - min]);
			}
			return minimum;
		}
	}
}
