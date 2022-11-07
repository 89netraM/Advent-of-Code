using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;

namespace AoC.Year2016;

[Day(22)]
public class Day22
{
	[Part(1)]
	public object Part1(string input)
	{
		var nodes = input
			.Lines()
			.Skip(2)
			.Extract<(Vector2 pos, long size, long used)>(@"(x(\d+)-y(\d+))\s+(\d+)T\s+(\d+)T")
			.ToDictionary(t => t.pos, t => (t.size, t.used));

		var count = 0;
		foreach (var (aPos, a) in nodes)
		{
			foreach (var (bPos, b) in nodes)
			{
				if (aPos != bPos && a.used > 0 && (b.size - b.used) >= a.used)
				{
					count++;
				}
			}
		}
		return count;
	}

	[Part(2)]
	public object Part2(string input)
	{
		var map = input
			.Lines()
			.Skip(2)
			.Extract<(Vector2 pos, (long size, long used) data)>(@"(x(\d+)-y(\d+))\s+((\d+)T\s+(\d+))T")
			.ToImmutableDictionary(t => t.pos, t => t.data);

		var (target, _) = map.Where(kvp => kvp.Key.Y == 0).MaxBy(kvp => kvp.Key.X);
		var (empty, _) = map.First(kvp => kvp.Value.used == 0);

		BFS.Search(
			(target, empty, map),
			FindMoves,
			p => p.target == Vector2.Zero,
			out var path,
			nodeComparer: new NodeComparer());

		return path.Count();
	}

	private IEnumerable<(Vector2, Vector2, ImmutableDictionary<Vector2, (long, long)>)> FindMoves((Vector2, Vector2, ImmutableDictionary<Vector2, (long size, long used)>) currentState)
	{
		var (target, empty, map) = currentState;
		var e = map[empty];
		foreach (var toPos in empty.NeighborsVonNeumann())
		{
			if (map.TryGetValue(toPos, out var to))
			{
				if (to.used <= e.size)
				{
					yield return (
						toPos == target ? empty : target,
						toPos,
						map.SetItem(empty, (e.size, to.used))
							.SetItem(toPos, (to.size, 0))
					);
				}
			}
		}
	}

	private class NodeComparer : IEqualityComparer<(Vector2 target, Vector2 empty, ImmutableDictionary<Vector2, (long, long)>)>
	{
		public bool Equals((Vector2 target, Vector2 empty, ImmutableDictionary<Vector2, (long, long)>) a, (Vector2 target, Vector2 empty, ImmutableDictionary<Vector2, (long, long)>) b) =>
			a.target == b.target && a.empty == b.empty;

		public int GetHashCode([DisallowNull] (Vector2 target, Vector2 empty, ImmutableDictionary<Vector2, (long, long)>) node) =>
			HashCode.Combine(node.target, node.empty);
	}
}
