using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2015
{
	[Day(17)]
	public class Day17
	{
		[Part(1)]
		public object Part1(string input) =>
			CountSums(input.Lines()
					.Select(Int64.Parse)
					.ToArray())
				.Count(p => p.s == 150);

		[Part(2)]
		public object Part2(string input) =>
			CountSums(input.Lines()
					.Select(Int64.Parse)
					.ToArray())
				.Where(p => p.s == 150)
				.GroupBy(p => p.c)
				.MinBy(g => g.Key)
				.Count();

		private IEnumerable<(long s, long c)> CountSums(long[] values)
		{
			if (values.Length == 0)
			{
				yield return (0, 0);
			}
			else
			{
				foreach (var (innerSum, innerCount) in CountSums(values[1..]))
				{
					yield return (innerSum, innerCount);
					yield return (values[0] + innerSum, innerCount + 1);
				}
			}
		}
	}
}
