#pragma warning disable CS8509

using System;
using AoC.Library;

namespace AoC.Year2016;

[Day(2)]
public class Day2
{
	[Part(1)]
	public object Part1(string input)
	{
		var pos = Vector2.Zero;
		var code = 0L;
		foreach (var line in input.Lines())
		{
			foreach (var d in line)
			{
				var nextPos = pos + d switch
				{
					'U' => Vector2.Up,
					'L' => Vector2.Left,
					'R' => Vector2.Right,
					'D' => Vector2.Down,
				};
				if (Math.Abs(nextPos.X) < 2 && Math.Abs(nextPos.Y) < 2)
				{
					pos = nextPos;
				}
			}
			code = code * 10 + PosToNum(pos);
		}
		return code;
	}

	long PosToNum(Vector2 pos) =>
		pos switch
		{
			(-1, -1) => 1,
			(0, -1) => 2,
			(1, -1) => 3,
			(-1, -0) => 4,
			(0, -0) => 5,
			(1, -0) => 6,
			(-1, 1) => 7,
			(0, 1) => 8,
			(1, 1) => 9,
		};

	[Part(2)]
	public object Part2(string input)
	{
		var pos = Vector2.Zero;
		var code = "";
		foreach (var line in input.Lines())
		{
			foreach (var d in line)
			{
				var nextPos = pos + d switch
				{
					'U' => Vector2.Up,
					'L' => Vector2.Left,
					'R' => Vector2.Right,
					'D' => Vector2.Down,
				};
				if (Vector2.Zero.ManhattanDistance(nextPos) <= 2)
				{
					pos = nextPos;
				}
			}
			code += PosToNumStar(pos);
		}
		return code;
	}

	char PosToNumStar(Vector2 pos) =>
		pos switch
		{
			(0, -2) => '1',
			(-1, -1) => '2',
			(0, -1) => '3',
			(1, -1) => '4',
			(-2, 0) => '5',
			(-1, 0) => '6',
			(0, 0) => '7',
			(1, 0) => '8',
			(2, 0) => '9',
			(-1, 1) => 'A',
			(0, 1) => 'B',
			(1, 1) => 'C',
			(0, 2) => 'D',
		};
}
