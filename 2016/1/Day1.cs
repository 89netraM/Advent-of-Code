using System.Collections.Generic;
using AoC.Library;
using RegExtract;

namespace AoC.Year2016;

[Day(1)]
public class Day1
{
	[Part(1)]
	public object Part1(string input)
	{
		var pos = Vector2.Zero;
		var dir = Vector2.Up;
		foreach (var (d, steps) in input.Split(", ").Extract<(char, long)>(@"(L|R)(\d+)"))
		{
			dir = d == 'L' ? new(-dir.Y, dir.X) : new(dir.Y, -dir.X);
			pos += dir * steps;
		}
		return Vector2.Zero.ManhattanDistance(pos);
	}

	[Part(2)]
	public object Part2(string input)
	{
		var pos = Vector2.Zero;
		var dir = Vector2.Up;
		var places = new HashSet<Vector2>();
		places.Add(pos);
		foreach (var (d, steps) in input.Split(", ").Extract<(char, long)>(@"(L|R)(\d+)"))
		{
			dir = d == 'L' ? new(-dir.Y, dir.X) : new(dir.Y, -dir.X);
			for (var s = 0; s < steps; s++)
			{
				pos += dir;
				if (!places.Add(pos))
				{
					return Vector2.Zero.ManhattanDistance(pos);
				}
			}
		}
		return null;
	}
}
