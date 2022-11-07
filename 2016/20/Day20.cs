using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2016;

[Day(20)]
public class Day20
{
	[Part(1)]
	public object Part1(string input)
	{
		var ranges = input
			.Lines()
			.Extract<(long l, long u)>(@"(\d+)-(\d+)")
			.OrderBy(p => p.l)
			.ToArray();

		long lowest = 0;
		for (int i = 0; i < ranges.Length; i++)
		{
			var (l, u) = ranges[i];
			if (l <= lowest && lowest <= u)
			{
				lowest = u + 1;
			}
		}
		return lowest;
	}

	[Part(2)]
	public object Part2(string input)
	{
		var ranges = input
			.Lines()
			.Extract<(long l, long u)>(@"(\d+)-(\d+)")
			.OrderBy(p => p.l)
			.ToArray();

		long count = 0;
		for (long lowest = 0; lowest <= 4294967295; lowest++, count++)
		{
			for (int i = 0; i < ranges.Length; i++)
			{
				var (l, u) = ranges[i];
				if (l <= lowest && lowest <= u)
				{
					lowest = u + 1;
				}
			}
		}
		return count - 1;
	}
}
