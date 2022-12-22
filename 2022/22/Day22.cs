using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2022;

#pragma warning disable CS8509

[Day(22)]
public class Day22
{
	[Part(1)]
	public long Part1(string input)
	{
		return Solve(input, Wrap);

		(Vector2, Vector2) Wrap(Vector2 pos, Vector2 dir, IReadOnlyDictionary<Vector2, Tile> map) =>
			dir switch
			{
				(1, 0) => (map.Keys.Where(p => p.Y == pos.Y).MinBy(p => p.X), dir),
				(0, 1) => (map.Keys.Where(p => p.X == pos.X).MinBy(p => p.Y), dir),
				(-1, 0) => (map.Keys.Where(p => p.Y == pos.Y).MaxBy(p => p.X), dir),
				(0, -1) => (map.Keys.Where(p => p.X == pos.X).MaxBy(p => p.Y), dir),
			};
	}

	[Part(2)]
	public long Part2(string input)
	{
		return Solve(input, Wrap);

		(Vector2, Vector2) Wrap(Vector2 pos, Vector2 dir, IReadOnlyDictionary<Vector2, Tile> map) =>
			dir switch
			{
				(1, 0) when pos.Y < 50 => (new Vector2(99, 149 - (pos.Y - 0)), Vector2.Left),
				(1, 0) when pos.Y < 100 => (new Vector2(100 + (pos.Y - 50), 49), Vector2.Up),
				(1, 0) when pos.Y < 150 => (new Vector2(149, 49 - (pos.Y - 100)), Vector2.Left),
				(1, 0) when pos.Y < 200 => (new Vector2(50 + (pos.Y - 150), 149), Vector2.Up),
				(0, 1) when pos.X < 50 => (new Vector2(100 + (pos.X - 0), 0), Vector2.Down),
				(0, 1) when pos.X < 100 => (new Vector2(49, 150 + (pos.X - 50)), Vector2.Left),
				(0, 1) when pos.X < 150 => (new Vector2(99, 50 + (pos.X - 100)), Vector2.Left),
				(-1, 0) when pos.Y < 50 => (new Vector2(0, 100 + (49 - pos.Y)), Vector2.Right),
				(-1, 0) when pos.Y < 100 => (new Vector2(pos.Y - 50, 100), Vector2.Down),
				(-1, 0) when pos.Y < 150 => (new Vector2(50, 49 - (pos.Y - 100)), Vector2.Right),
				(-1, 0) when pos.Y < 200 => (new Vector2(pos.Y - 100, 0), Vector2.Down),
				(0, -1) when pos.X < 50 => (new Vector2(50, 50 + pos.X), Vector2.Right),
				(0, -1) when pos.X < 100 => (new Vector2(0, pos.X + 100), Vector2.Right),
				(0, -1) when pos.X < 150 => (new Vector2(pos.X - 100, 199), Vector2.Up),
			};
	}

	private long Solve(string input, Func<Vector2, Vector2, IReadOnlyDictionary<Vector2, Tile>, (Vector2, Vector2)> wrap)
	{
		var parts = input.Split("\n\n");
		var map = parts[0].ToMap()
			.Where(kvp => kvp.Value != ' ')
			.ToDictionary(kvp => kvp.Key, kvp => kvp.Value == '.' ? Tile.Open : Tile.Wall);
		var instructions = parts[1].AdjacentGroupBy(Char.IsAsciiDigit).Select(g => String.Concat(g));

		var pos = map.Keys.Where(p => p.Y == 0).MinBy(p => p.X);
		var dir = Vector2.Right;

		foreach (var instruction in instructions)
		{
			if (long.TryParse(instruction, out var steps))
			{
				for (long s = 0; s < steps; s++)
				{
					var (nextPos, nextDir) = (pos + dir, dir);
					if (!map.ContainsKey(nextPos))
					{
						(nextPos, nextDir) = wrap(pos, dir, map);
					}
					if (map[nextPos] == Tile.Open)
					{
						(pos, dir) = (nextPos, nextDir);
					}
					else
					{
						break;
					}
				}
			}
			else
			{
				dir = dir.Rotate(instruction == "R" ? 1 : -1);
			}
		}

		return 1000 * (pos.Y + 1) + 4 * (pos.X + 1) + (dir switch { (1, 0) => 0, (0, 1) => 1, (-1, 0) => 2, (0, -1) => 3 });
	}

	private enum Tile
	{
		Wall,
		Open,
	}
}
