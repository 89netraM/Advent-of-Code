using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;

namespace AoC.Year2015
{
	[Day(2)]
	public class Day2
	{
		[Part(1)]
		public object Part1(string input)
		{
			return input.Lines().Select(static l =>
			{
				var dims = l.Split('x').Select(long.Parse);
				var areas = dims.Zip(dims.Skip(1).Concat(dims), static (l, w) => l * w);
				return 2 * areas.Sum() + areas.Min();
			}).Sum();
		}

		[Part(2)]
		public object Part2(string input)
		{
			return input.Lines().Select(static l =>
			{
				var dims = l.Split('x').Select(long.Parse);
				var minCircumfens = dims.OrderBy(Id).Take(2).Sum() * 2;
				return minCircumfens + dims.Aggregate(1L, static (a, b) => a * b);
			}).Sum();
		}
	}
}
