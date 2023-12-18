using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;
using System.Globalization;

namespace AoC.Year2023;

[Day(18)]
public class Day18
{
	[Part(1)]
	public object Part1(string input)
	{
		var pos = Vector2.Zero;
		var map = new List<(Vector2, Vector2)>();
		foreach (var (dir, meters, _) in Parse(input))
		{
			var delta = dir switch
			{
				Direction.U => Vector2.Up,
				Direction.L => Vector2.Left,
				Direction.R => Vector2.Right,
				Direction.D => Vector2.Down,
				_ => throw new Exception("No"),
			};
			map.Add((pos, pos + delta * meters));
			pos += delta * meters;
		}
		return CountInside(map);
	}

	[Part(2)]
	public object Part2(string input)
	{
		var pos = Vector2.Zero;
		var map = new List<(Vector2, Vector2)>();
		foreach (var (_, _, hex) in Parse(input))
		{
			var meters = long.Parse(hex[..5], NumberStyles.HexNumber);
			var dir = (Direction)int.Parse(hex[5].ToString(), NumberStyles.HexNumber);
			var delta = dir switch
			{
				Direction.U => Vector2.Up,
				Direction.L => Vector2.Left,
				Direction.R => Vector2.Right,
				Direction.D => Vector2.Down,
				_ => throw new Exception("No"),
			};
			map.Add((pos, pos + delta * meters));
			pos += delta * meters;
		}
		return CountInside(map);
	}

	private static long CountInside(IReadOnlyCollection<(Vector2 from, Vector2 to)> map) =>
		map.Sum(p => p.from.ManhattanDistance(p.to)) / 2
			+ map.Sum(p => (p.from.Y + p.to.Y) * (p.from.X - p.to.X)) / 2
			+ 1;

	private static IEnumerable<DigPlan> Parse(string input) =>
		input.Lines()
			.Extract<DigPlan>(@"(.) (\d+) \(#(.*?)\)");

	private record DigPlan(Direction Dir, long Meters, string Hex);
	private enum Direction : int
	{
		R = 0,
		D = 1,
		L = 2,
		U = 3,
	}
}
