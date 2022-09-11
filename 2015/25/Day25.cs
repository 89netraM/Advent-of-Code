using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2015
{
	[Day(25)]
	public class Day25
	{
		private static readonly IReadOnlyDictionary<(int r, int c), ulong> Codes = new Dictionary<(int, int), ulong>
		{
			[(1, 1)] = 20151125,
			[(1, 2)] = 18749137,
			[(1, 3)] = 17289845,
			[(1, 4)] = 30943339,
			[(1, 5)] = 10071777,
			[(1, 6)] = 33511524,
			[(2, 1)] = 31916031,
			[(2, 2)] = 21629792,
			[(2, 3)] = 16929656,
			[(2, 4)] = 7726640,
			[(2, 5)] = 15514188,
			[(2, 6)] = 4041754,
			[(3, 1)] = 16080970,
			[(3, 2)] = 8057251,
			[(3, 3)] = 1601130,
			[(3, 4)] = 7981243,
			[(3, 5)] = 11661866,
			[(3, 6)] = 16474243,
			[(4, 1)] = 24592653,
			[(4, 2)] = 32451966,
			[(4, 3)] = 21345942,
			[(4, 4)] = 9380097,
			[(4, 5)] = 10600672,
			[(4, 6)] = 31527494,
			[(5, 1)] = 77061,
			[(5, 2)] = 17552253,
			[(5, 3)] = 28094349,
			[(5, 4)] = 6899651,
			[(5, 5)] = 9250759,
			[(5, 6)] = 31663883,
			[(6, 1)] = 33071741,
			[(6, 2)] = 6796745,
			[(6, 3)] = 25397450,
			[(6, 4)] = 24659492,
			[(6, 5)] = 1534922,
			[(6, 6)] = 27995004,
		};

		[Part(1)]
		public object Part1(string input)
		{
			var (row, column) = input.Extract<(int, int)>(@"row (\d+), column (\d+)");
			var index = IndexOfCoord(row, column);

			var (closestIndex, value) = Codes
				.Select(kvp => (i: IndexOfCoord(kvp.Key.r, kvp.Key.c), kvp.Value))
				.Where(p => p.i < index)
				.MinBy(p => index - p.i);
			for (int i = closestIndex; i < index; i++)
			{
				value = (value * 252533UL) % 33554393UL;
			}
			return value;
		}

		private int IndexOfCoord(int row, int column) =>
			Sum(row + column - 2) + column;

		private int Sum(int l) =>
			l * (l + 1) / 2;
	}
}
