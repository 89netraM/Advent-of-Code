using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using System.Collections.Immutable;

namespace AoC.Year2023;

[Day(23)]
public class Day23
{
	[Part(1)]
	public object Part1(string input)
	{
		var map = input.ToMap();
		var max = map.Keys.Aggregate((a, b) => a.MaxParts(b));
		var start = map.Single(kvp => kvp.Key.Y == 0 && kvp.Value != '#').Key;
		var end = map.Single(kvp => kvp.Key.Y == max.Y && kvp.Value != '#').Key;

		var neighbors = ReduceMap(
			start,
			end,
			c => c.NeighborsVonNeumann()
				.Where(n => map.TryGetValue(n, out var t) && t != '#'
					&& (t != '^' || (n - c) == Vector2.Up)
					&& (t != '>' || (n - c) == Vector2.Right)
					&& (t != '<' || (n - c) == Vector2.Left)
					&& (t != 'v' || (n - c) == Vector2.Down)));

		return Longest(start);

		long Longest(Vector2 from) =>
			from == end
				? 0
				: neighbors[from].Max(p => Longest(p.Item1) + p.Item2);
	}

	[Part(2)]
	public object Part2(string input)
	{
		var map = input.ToMap();
		var max = map.Keys.Aggregate((a, b) => a.MaxParts(b));
		var start = map.Single(kvp => kvp.Key.Y == 0 && kvp.Value != '#').Key;
		var end = map.Single(kvp => kvp.Key.Y == max.Y && kvp.Value != '#').Key;

		var neighbors = ReduceMap(
			start,
			end,
			c => c.NeighborsVonNeumann()
				.Where(n => map.TryGetValue(n, out var t) && t != '#'));

		return Longest(start, ImmutableHashSet.Create<Vector2>());

		long Longest(Vector2 from, IImmutableSet<Vector2> visited) =>
			from == end
				? 0
				: neighbors[from]
					.Where(p => !visited.Contains(p.Item1))
					.Let(ns => ns.Any()
						? ns.Max(p => Longest(p.Item1, visited.Add(p.Item1)) + p.Item2)
						: long.MinValue);
	}

	private static Dictionary<Vector2, List<(Vector2, long)>> ReduceMap(Vector2 start, Vector2 end, Func<Vector2, IEnumerable<Vector2>> getNexts)
	{
		var froms = new Queue<Vector2>();
		froms.Enqueue(start);
		var neighbors = new Dictionary<Vector2, List<(Vector2, long)>>();
		while (froms.TryDequeue(out var from))
		{
			if (neighbors.ContainsKey(from)) { continue; }
			var ns = new List<(Vector2, long)>();
			foreach (var n in getNexts(from))
			{
				var to = GoStraight(from, n, end, getNexts);
				if (to.HasValue)
				{
					ns.Add(to.Value);
					if (to.Value.pos != end)
					{
						froms.Enqueue(to.Value.pos);
					}
				}
			}
			neighbors.Add(from, ns);
		}
		return neighbors;
	}

	private static (Vector2 pos, long distance)? GoStraight(Vector2 origin, Vector2 from, Vector2 end, Func<Vector2, IEnumerable<Vector2>> getNexts)
	{
		var visited = new HashSet<Vector2> { origin };
		var toVisit = new Queue<(Vector2, long)>();
		toVisit.Enqueue((from, 1));
		while (toVisit.TryDequeue(out var current))
		{
			var (node, distance) = current;
			visited.Add(node);
			var nexts = getNexts(node).Where(n => !visited.Contains(n)).ToArray();
			if (nexts is [var next])
			{
				toVisit.Enqueue((next, distance + 1));
			}
			else if (nexts.Length > 0 || current.Item1 == end)
			{
				return current;
			}
		}
		return null;
	}
}
