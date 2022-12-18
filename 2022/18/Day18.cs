using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2022;

[Day(18)]
public class Day18
{
	[Part(1)]
	public object Part1(string input)
	{
		var lava = input.Lines()
			.Extract<Vector3>(@"(\d+),(\d+),(\d+)")
			.ToHashSet();
		
		return lava.Sum(d => d.NeighborsVonNeumann().Count(n => !lava.Contains(n)));
	}

	[Part(2)]
	public object Part2(string input)
	{
		var lava = input.Lines()
			.Extract<Vector3>(@"(\d+),(\d+),(\d+)")
			.ToHashSet();

		var min = lava.Aggregate((m, n) => n.MinParts(m));
		var max = lava.Aggregate((m, n) => n.MaxParts(m));
		
		return lava.Sum(d => d.NeighborsVonNeumann()
				.Count(n => !lava.Contains(n) &&
					BFS.Search(
						n,
						n => n.NeighborsVonNeumann().Where(n => !lava.Contains(n)),
						n => n.Zip(min).Any(p => p.First < p.Second) || n.Zip(max).Any(p => p.First > p.Second),
						out _)));
	}
}
