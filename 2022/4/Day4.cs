using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2022;

[Day(4)]
public class Day4
{
	[Part(1)]
	public object Part1(string input) =>
		input.Lines()
			.Select(pair => pair.Split(",").Extract<(long, long)>(@"(\d+)-(\d+)"))
			.Count(Contains);

	[Part(2)]
	public object Part2(string input) =>
		input.Lines()
			.Select(pair => pair.Split(",").Extract<(long, long)>(@"(\d+)-(\d+)"))
			.Count(Overlaps);

	private bool Contains(IEnumerable<(long l, long r)> pairs)
	{
		var first = pairs.First();
		var second = pairs.Last();
		if (Size(second) > Size(first))
		{
			(first, second) = (second, first);
		}
		return first.l <= second.l && second.r <= first.r;
	}

	private bool Overlaps(IEnumerable<(long l, long r)> pairs)
	{
		var first = pairs.First();
		var second = pairs.Last();

		return (first.l <= second.l && second.l <= first.r) ||
			(first.l <= second.r && second.r <= first.r) ||
			(second.l <= first.l && first.l <= second.r) ||
			(second.l <= first.r && first.r <= second.r);
	}

	private long Size((long l, long r) range) =>
		range.r - range.l;
}
