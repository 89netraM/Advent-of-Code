using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;

namespace AoC.Year2016;

[Day(24)]
public class Day24
{
	[Part(1)]
	public object Part1(string input)
	{
		var map = input.ToMap(c => c == '#' ? Tile.Wall : Tile.Floor);
		var targets = input.ToMap(c => Char.IsNumber(c)).Where(kvp => kvp.Value).Select(kvp => kvp.Key).ToHashSet();
		var start = input.ToMap().First(kvp => kvp.Value == '0').Key;

		BFS.Search(
			(start, set: ImmutableHashSet.Create(start)),
			Finder(map, targets),
			p => targets.All(p.set.Contains),
			out var path,
			nodeComparer: new NodeComparer());

		return path.Count();
	}

	[Part(2)]
	public object Part2(string input)
	{
		var map = input.ToMap(c => c == '#' ? Tile.Wall : Tile.Floor);
		var targets = input.ToMap(c => Char.IsNumber(c)).Where(kvp => kvp.Value).Select(kvp => kvp.Key).ToHashSet();
		var start = input.ToMap().First(kvp => kvp.Value == '0').Key;

		BFS.Search(
			(pos: start, set: ImmutableHashSet.Create(start)),
			Finder(map, targets),
			p => p.pos == start && targets.All(p.set.Contains),
			out var path,
			nodeComparer: new NodeComparer());

		return path.Count();
	}

	private Func<(Vector2 pos, ImmutableHashSet<Vector2> set), IEnumerable<(Vector2 pos, ImmutableHashSet<Vector2> set)>> Finder(IDictionary<Vector2, Tile> map, ISet<Vector2> targets)
	{
		return FindNexts;

		IEnumerable<(Vector2, ImmutableHashSet<Vector2>)> FindNexts((Vector2 pos, ImmutableHashSet<Vector2> set) node)
		{
			foreach (var neighbor in node.pos.NeighborsVonNeumann())
			{
				if (map.TryGetValue(neighbor, out var tile) && tile == Tile.Floor)
				{
					if (targets.Contains(neighbor))
					{
						yield return (neighbor, node.set.Add(neighbor));
					}
					else
					{
						yield return (neighbor, node.set);
					}
				}
			}
		}
	}

	enum Tile
	{
		Wall,
		Floor,
	}

	private class NodeComparer : IEqualityComparer<(Vector2 pos, ImmutableHashSet<Vector2> set)>
	{
		public bool Equals((Vector2 pos, ImmutableHashSet<Vector2> set) a, (Vector2 pos, ImmutableHashSet<Vector2> set) b)
		{
			if (a.pos != b.pos)
			{
				return false;
			}
			else
			{
				foreach (var t in a.set)
				{
					if (!b.set.Contains(t))
					{
						return false;
					}
				}
				foreach (var t in b.set)
				{
					if (!a.set.Contains(t))
					{
						return false;
					}
				}

				return true;
			}
		}

		public int GetHashCode([DisallowNull] (Vector2 pos, ImmutableHashSet<Vector2> set) node) =>
			node.pos.GetHashCode();
	}
}
