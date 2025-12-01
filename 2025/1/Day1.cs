using System.Linq;
using AoC.Library;

namespace AoC.Year2025;

[Day(1)]
public class Day1
{
	[Part(1)]
	public object Part1(string input) =>
		input.Lines()
			.Select(l => l[0] is 'R' ? long.Parse(l[1..]) : -long.Parse(l[1..]))
			.Aggregate((pos: 50L, count: 0L), (p, r) => (MathM.Mod(p.pos + r, 100), p.count + (MathM.Mod(p.pos + r, 100) is 0 ? 1 : 0)))
			.count;

	[Part(2)]
	public object Part2(string input) =>
		input.Lines()
			.Select(l => l[0] is 'R' ? long.Parse(l[1..]) : -long.Parse(l[1..]))
			.SelectMany(r => Enumerable.Repeat(long.Sign(r), int.Abs((int)r)))
			.Aggregate((pos: 50L, count: 0L), (p, r) => (MathM.Mod(p.pos + r, 100), p.count + (MathM.Mod(p.pos + r, 100) is 0 ? 1 : 0)))
			.count;
}
