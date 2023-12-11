using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2023;

[Day(11)]
public class Day11
{
	[Part(1)]
	public object Part1(string input)
	{
		var map = input.ToMap()
			.Where(kvp => kvp.Value != '.')
			.Select(kvp => kvp.Key)
			.ToArray()
			.Let(Expand);

		return PairWiseDistances(map);
	}

	[Part(2)]
	public object Part2(string input)
	{
		var map = input.ToMap()
			.Where(kvp => kvp.Value != '.')
			.Select(kvp => kvp.Key)
			.ToArray()
			.Let(a => Expand(a, 1000000));

		return PairWiseDistances(map);
	}

	private static long PairWiseDistances(IReadOnlyList<Vector2> map)
	{
		long steps = 0;
		for (int i = 0; i < map.Count; i++)
		{
			for (int j = i + 1; j < map.Count; j++)
			{
				steps += map[i].ManhattanDistance(map[j]);
			}
		}
		return steps;
	}

	private static List<Vector2> Expand(IReadOnlyCollection<Vector2> map) =>
		ExpandRows(ExpandColumns(map, 2), 2);
	private static List<Vector2> Expand(IReadOnlyCollection<Vector2> map, long expansion) =>
		ExpandRows(ExpandColumns(map, expansion), expansion);
	private static List<Vector2> ExpandColumns(IReadOnlyCollection<Vector2> map, long expansion)
	{
		var outMap = new List<Vector2>();
		long offset = 0;
		var min = map.Aggregate((a, b) => a.MinParts(b));
		var max = map.Aggregate((a, b) => a.MaxParts(b));
		for (long x = min.X; x <= max.X; x++)
		{
			var column = map.Where(c => c.X == x).ToArray();
			if (column.Length == 0)
			{
				offset += expansion - 1;
			}
			else
			{
				outMap.AddRange(column.Select(c => new Vector2(c.X + offset, c.Y)));
			}
		}
		return outMap;
	}
	private static List<Vector2> ExpandRows(IReadOnlyCollection<Vector2> map, long expansion)
	{
		var outMap = new List<Vector2>();
		long offset = 0;
		var min = map.Aggregate((a, b) => a.MinParts(b));
		var max = map.Aggregate((a, b) => a.MaxParts(b));
		for (long y = min.Y; y <= max.Y; y++)
		{
			var row = map.Where(c => c.Y == y).ToArray();
			if (row.Length == 0)
			{
				offset += expansion - 1;
			}
			else
			{
				outMap.AddRange(row.Select(c => new Vector2(c.X, c.Y + offset)));
			}
		}
		return outMap;
	}
}
