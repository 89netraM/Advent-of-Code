using System;
using System.Linq;
using AoC.Library;

namespace AoC.Year2022;

[Day(12)]
public class Day12
{
	[Part(1)]
	public object Part1(string input)
	{
		var map = input.ToMap();
		var start = map.Single(kvp => kvp.Value == 'S').Key;
		map[start] = 'a';
		var end = map.Single(kvp => kvp.Value == 'E').Key;
		map[end] = 'z';

		BFS.Search(
			start,
			f => f.NeighborsVonNeumann().Where(n => map.ContainsKey(n) && (map[n] - map[f]) <= 1),
			f => f == end,
			out var path);
		return path.Count();
	}

	[Part(2)]
	public object Part2(string input)
	{
		var map = input.ToMap();
		var start = map.Single(kvp => kvp.Value == 'S').Key;
		map[start] = 'a';
		var end = map.Single(kvp => kvp.Value == 'E').Key;
		map[end] = 'z';

		long shortest = long.MaxValue;
		foreach (var a in map.Where(kvp => kvp.Value == 'a').Select(kvp => kvp.Key))
		{
			if (BFS.Search(
				a,
				f => f.NeighborsVonNeumann().Where(n => map.ContainsKey(n) && (map[n] - map[f]) <= 1),
				f => f == end,
				out var path))
			{
				shortest = Math.Min(path.Count(), shortest);
			}
		}
		return shortest;
	}
}
