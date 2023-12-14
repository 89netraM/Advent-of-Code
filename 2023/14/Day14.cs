using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;

namespace AoC.Year2023;

[Day(14)]
public class Day14
{
	[Part(1)]
	public object Part1(string input)
	{
		return input.ToMap()
			.Where(kvp => kvp.Value != '.')
			.ToDictionary()
			.Let(Tilt)
			.Let(CalculateLoad);
	}

	private static Dictionary<Vector2, char> Tilt(IReadOnlyDictionary<Vector2, char> map)
	{
		var outMap = map.Where(kvp => kvp.Value == '#').ToDictionary();
		var max = map.Keys.Aggregate((a, b) => a.MaxParts(b));

		for (long y = 0; y <= max.Y; y++)
		for (long x = 0; x <= max.X; x++)
		{
			var rock = new Vector2(x, y);
			if (!map.TryGetValue(rock, out var c) || c != 'O') { continue; }
			while (true)
			{
				var next = rock + Vector2.Up;
				if (next.Y < 0 || outMap.ContainsKey(next))
				{
					outMap.Add(rock, 'O');
					break;
				}
				rock = next;
			}
		}

		return outMap;
	}

	private static long CalculateLoad(IReadOnlyDictionary<Vector2, char> map)
	{
		long load = 0;
		var max = map.Keys.Aggregate((a, b) => a.MaxParts(b));

		foreach (var (rock, _) in map.Where(kvp => kvp.Value == 'O'))
		{
			load += max.Y + 1 - rock.Y;
		}

		return load;
	}

	[Part(2)]
	public object Part2(string input)
	{
		var rocks = input.ToMap()
			.Where(kvp => kvp.Value == 'O')
			.Select(kvp => kvp.Key)
			.ToImmutableHashSet();
		var squares = input.ToMap()
			.Where(kvp => kvp.Value == '#')
			.Select(kvp => kvp.Key)
			.ToImmutableHashSet();

		var max = rocks.Aggregate((a, b) => a.MaxParts(b)).MaxParts(squares.Aggregate((a, b) => a.MaxParts(b)));

		return Huge.TakeImmutableSteps(
			1000000000L,
			rocks,
			(rocks, _) => Spin(squares, max, rocks),
			(rocks, _) => CalculateLoad(rocks, max.Y),
			new RocksComparer(),
			HugeSearchPattern.Period);
	}

	private static ImmutableHashSet<Vector2> Spin(ImmutableHashSet<Vector2> squares, Vector2 max, ImmutableHashSet<Vector2> rocks)
	{
		return rocks
			.Let(TiltNorth)
			.Let(TiltWest)
			.Let(TiltSouth)
			.Let(TiltEast);

		ImmutableHashSet<Vector2> TiltNorth(ImmutableHashSet<Vector2> rocks)
		{
			var builder = ImmutableHashSet.CreateBuilder<Vector2>();

			for (long y = 0; y <= max.Y; y++)
			for (long x = 0; x <= max.X; x++)
			{
				var rock = new Vector2(x, y);
				if (!rocks.Contains(rock)) { continue; }
				while (true)
				{
					var next = rock + Vector2.Up;
					if (next.Y < 0 || squares.Contains(next) || builder.Contains(next))
					{
						builder.Add(rock);
						break;
					}
					rock = next;
				}
			}

			return builder.ToImmutable();
		}
		ImmutableHashSet<Vector2> TiltWest(ImmutableHashSet<Vector2> rocks)
		{
			var builder = ImmutableHashSet.CreateBuilder<Vector2>();

			for (long x = 0; x <= max.X; x++)
			for (long y = 0; y <= max.Y; y++)
			{
				var rock = new Vector2(x, y);
				if (!rocks.Contains(rock)) { continue; }
				while (true)
				{
					var next = rock + Vector2.Left;
					if (next.X < 0 || squares.Contains(next) || builder.Contains(next))
					{
						builder.Add(rock);
						break;
					}
					rock = next;
				}
			}

			return builder.ToImmutable();
		}
		ImmutableHashSet<Vector2> TiltSouth(ImmutableHashSet<Vector2> rocks)
		{
			var builder = ImmutableHashSet.CreateBuilder<Vector2>();

			for (long y = max.Y; y >= 0; y--)
			for (long x = 0; x <= max.X; x++)
			{
				var rock = new Vector2(x, y);
				if (!rocks.Contains(rock)) { continue; }
				while (true)
				{
					var next = rock + Vector2.Down;
					if (next.Y > max.Y || squares.Contains(next) || builder.Contains(next))
					{
						builder.Add(rock);
						break;
					}
					rock = next;
				}
			}

			return builder.ToImmutable();
		}
		ImmutableHashSet<Vector2> TiltEast(ImmutableHashSet<Vector2> rocks)
		{
			var builder = ImmutableHashSet.CreateBuilder<Vector2>();

			for (long x = max.X; x >= 0; x--)
			for (long y = 0; y <= max.Y; y++)
			{
				var rock = new Vector2(x, y);
				if (!rocks.Contains(rock)) { continue; }
				while (true)
				{
					var next = rock + Vector2.Right;
					if (next.X > max.X || squares.Contains(next) || builder.Contains(next))
					{
						builder.Add(rock);
						break;
					}
					rock = next;
				}
			}

			return builder.ToImmutable();
		}
	}

	private static long CalculateLoad(ImmutableHashSet<Vector2> rocks, long bottom)
	{
		long load = 0;

		foreach (var rock in rocks)
		{
			load += bottom + 1 - rock.Y;
		}

		return load;
	}

	private class RocksComparer : IEqualityComparer<ImmutableHashSet<Vector2>>
	{
		public bool Equals(ImmutableHashSet<Vector2> x, ImmutableHashSet<Vector2> y) =>
			x.IsSubsetOf(y) && y.IsSubsetOf(x);

		public int GetHashCode([DisallowNull] ImmutableHashSet<Vector2> obj) =>
			obj.Aggregate(new HashCode(), (h, c) => h.Add(c))
				.ToHashCode();
	}
}
