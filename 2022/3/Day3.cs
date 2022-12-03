using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2022;

[Day(3)]
public class Day3
{
	[Part(1)]
	public object Part1(string input) =>
		input.Lines()
			.Sum(PrioOfItemInBothCompartments);

	private long PrioOfItemInBothCompartments(string sack)
	{
		var common = new HashSet<char>(sack.Substring(0, sack.Length / 2));
		common.IntersectWith(sack.Substring(sack.Length / 2));
		return PrioOf(common.Single());
	}

	[Part(2)]
	public object Part2(string input) =>
		input.Lines()
			.Chunk(3)
			.Sum(PrioOfBadges);

	private long PrioOfBadges(string[] sacks)
	{
		var common = new HashSet<char>(sacks[0]);
		common.IntersectWith(sacks[1]);
		common.IntersectWith(sacks[2]);
		return PrioOf(common.Single());
	}

	private long PrioOf(char c) =>
		c <= 'Z' ? (long)(c - 'A' + 27) : (long)(c - 'a' + 1);
}
