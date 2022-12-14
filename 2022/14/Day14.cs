using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2022;

[Day(14)]
public class Day14
{
	[Part(1)]
	public object Part1(string input)
	{
		var walls = input.Lines()
			.SelectMany(ParseWall)
			.ToHashSet();
		var lowest = walls.Max(p => p.Y);
		long sandCount = 0;

		while (true)
		{
			var sand = new Vector2(500, 0);
			while (true)
			{
				if (sand.Y > lowest)
				{
					return sandCount;
				}

				var next = sand + Vector2.Down;
				if (!walls.Contains(next))
				{
					sand = next;
					continue;
				}
				next += Vector2.Left;
				if (!walls.Contains(next))
				{
					sand = next;
					continue;
				}
				next += Vector2.Right * 2;
				if (!walls.Contains(next))
				{
					sand = next;
					continue;
				}
				break;
			}
			walls.Add(sand);
			sandCount += 1;
		}
	}

	[Part(2)]
	public object Part2(string input)
	{
		var walls = input.Lines()
			.SelectMany(ParseWall)
			.ToHashSet();
		var floor = walls.Max(p => p.Y) + 2;
		long sandCount = 0;

		while (true)
		{
			var sand = new Vector2(500, 0);
			if (walls.Contains(sand))
			{
				return sandCount;
			}
			while (true)
			{
				var next = sand + Vector2.Down;
				if (next.Y == floor)
				{
					break;
				}
				if (!walls.Contains(next))
				{
					sand = next;
					continue;
				}
				next += Vector2.Left;
				if (!walls.Contains(next))
				{
					sand = next;
					continue;
				}
				next += Vector2.Right * 2;
				if (!walls.Contains(next))
				{
					sand = next;
					continue;
				}
				break;
			}
			walls.Add(sand);
			sandCount += 1;
		}
	}

	private IEnumerable<Vector2> ParseWall(string input)
	{
		var points = input.Split(" -> ").Select(s => s.Split(',').Select(long.Parse).ToArray()).Select(p => new Vector2(p[0], p[1])).ToArray();
		var point = points[0];
		for (int i = 1; i < points.Length; i++)
		{
			var to = points[i];
			var dir = (to - point) / point.ManhattanDistance(to);
			for (; point != to; point += dir)
			{
				yield return point;
			}
		}
		yield return point;
	}
}
