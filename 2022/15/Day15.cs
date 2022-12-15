using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2022;

[Day(15)]
public class Day15
{
	[Part(1)]
	public object Part1(string input)
	{
		var nonBeacons = new HashSet<long>();

		const long YLINE = 2000000;
		foreach (var (s, b) in input.Lines().Extract<(Vector2, Vector2)>(@"(x=(-?\d+), y=(-?\d+)).*?(x=(-?\d+), y=(-?\d+))"))
		{
			var distance = s.ManhattanDistance(b);
			var y = Math.Abs(YLINE - s.Y);
			if (y < distance)
			{
				var xMin = s.X - (distance - y);
				var xMax = s.X + (distance - y);
				for (long x = xMin; x <= xMax; x++)
				{
					if (!(b.Y == YLINE && b.X == x))
					{
						nonBeacons.Add(x);
					}
				}
			}
		}

		return nonBeacons.Count;
	}

	[Part(2)]
	public object Part2(string input)
	{
		var sensors = input.Lines()
			.Extract<(Vector2 s, Vector2 b)>(@"(x=(-?\d+), y=(-?\d+)).*?(x=(-?\d+), y=(-?\d+))")
			.Select(p => (s: p.s, d: p.s.ManhattanDistance(p.b)))
			.ToArray();

		foreach (var (s, d) in sensors)
		{
			var edge = d + 1;
			for (long x = -edge; x <= edge; x++)
			{
				var y = edge - Math.Abs(x);
				var point = new Vector2(s.X + x, s.Y + y);
				if (IsInside(point) && sensors.All(p => p.s.ManhattanDistance(point) > p.d))
				{
					return point.X * 4000000 + point.Y;
				}
				point = new Vector2(point.X, s.Y - y);
				if (IsInside(point) && sensors.All(p => p.s.ManhattanDistance(point) > p.d))
				{
					return point.X * 4000000 + point.Y;
				}
			}
		}

		return null;
	}

	private bool IsInside(Vector2 point) =>
		IsInside(point.X) && IsInside(point.Y);
	private bool IsInside(long v) =>
		0 <= v && v <= 4000000;
}
