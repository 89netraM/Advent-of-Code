using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;
using RegExtract;
using System.Collections.Immutable;

namespace AoC.Year2022;

[Day(16)]
public class Day16
{

	[Part(1)]
	public object Part1(string input)
	{
		var map = input.Lines()
			.Extract<(string node, long pressure, string tos)>(@"Valve (..) has flow rate=(\d+); tunnels? leads? to valves? (.*)")
			.ToDictionary(t => t.node, t => (t.pressure, t.tos.Split(", ")));
		var paths = OptimizePaths(map);

		Func<string, long, ImmutableHashSet<string>, long> best = null;
		best = Curry(Memoize(Uncurry<string, long, ImmutableHashSet<string>, long>((from, minutes, taken) =>
		{
			var max = 0L;
			if (minutes == 0)
			{
				return max;
			}
			foreach (var (to, distance) in paths[from])
			{
				var afterMove = minutes - distance - 1;
				if (afterMove >= 0 && !taken.Contains(to))
				{
					max = Math.Max(max, best(to, afterMove, taken.Add(to)) + map[to].pressure * afterMove);
				}
			}
			return max;
		})));

		return best("AA", 30, ImmutableHashSet.Create<string>("AA"));
	}

	[Part(2)]
	public object Part2(string input)
	{
		var map = input.Lines()
			.Extract<(string node, long pressure, string tos)>(@"Valve (..) has flow rate=(\d+); tunnels? leads? to valves? (.*)")
			.ToDictionary(t => t.node, t => (t.pressure, tos: t.tos.Split(", ")));
		var paths = OptimizePaths(map);

		Func<string, long, ImmutableHashSet<string>, List<(long pressure, ImmutableHashSet<string> taken)>> allSolutions = null;
		allSolutions = Curry(Memoize(Uncurry<string, long, ImmutableHashSet<string>, List<(long, ImmutableHashSet<string>)>>((from, minutes, taken) =>
		{
			var l = new List<(long, ImmutableHashSet<string>)> { (0L, taken) };
			if (minutes == 0)
			{
				return l;
			}
			foreach (var (to, distance) in paths[from])
			{
				var afterMove = minutes - distance - 1;
				if (afterMove >= 0 && !taken.Contains(to))
				{
					foreach (var (nextPressure, nextTaken) in allSolutions(to, afterMove, taken.Add(to)))
					{
						l.Add((nextPressure + map[to].pressure * afterMove, nextTaken));
					}
				}
			}
			return l;
		})));
		Func<string, long, ImmutableHashSet<string>, (long pressure, ImmutableHashSet<string> taken)> best = null;
		best = Curry(Memoize(Uncurry<string, long, ImmutableHashSet<string>, (long, ImmutableHashSet<string>)>((from, minutes, taken) =>
		{
			var max = (pressure: 0L, taken);
			if (minutes == 0)
			{
				return max;
			}
			foreach (var (to, distance) in paths[from])
			{
				var afterMove = minutes - distance - 1;
				if (afterMove >= 0 && !taken.Contains(to))
				{
					var (nextPressure, nextTaken) = best(to, afterMove, taken.Add(to));
					nextPressure += map[to].pressure * afterMove;
					if (nextPressure > max.pressure)
					{
						max = (nextPressure, nextTaken);
					}
				}
			}
			return max;
		})));

		var max = 0L;
		var me = allSolutions("AA", 26, ImmutableHashSet.Create<string>());
		for (int i = 0; i < me.Count; i++)
		{
			var (pressure, mine) = me[i];
			if (i % 100 == 0)
			{
				Console.WriteLine($"{i.ToString().PadLeft((int)Math.Log10(me.Count))}/{me.Count}: {max}");
			}
			max = Math.Max(max, best("AA", 26, mine).pressure + pressure);
		}
		return max;
	}

	private Dictionary<string, Dictionary<string, long>> OptimizePaths(Dictionary<string, (long pressure, string[] tos)> map)
	{
		var optMap = new Dictionary<string, Dictionary<string, long>>();

		foreach (var from in map.Where(kvp => kvp.Value.pressure > 0).Select(kvp => kvp.Key).Append("AA"))
		{
			var to = new Dictionary<string, long>();
			BFS.Search(
				from,
				n => map[n].tos.Select(n => (n, 1L)),
				_ => false,
				out _,
				between: (n, d) => { if (map[n].pressure > 0) { to.Add(n, d); } });
			optMap[from] = to;
		}

		return optMap;
	}
}
