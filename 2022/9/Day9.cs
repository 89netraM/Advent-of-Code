using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2022;

[Day(9)]
public class Day9
{
	[Part(1)]
	public object Part1(string input)
	{
		var tail = Vector2.Zero;
		var head = Vector2.Zero;
		var positions = new HashSet<Vector2> { tail };

		foreach (var (d, dist) in input.Lines().Extract<(char, int)>(@"(.) (\d+)"))
		{
			var dir = d switch
			{
				'R' => Vector2.Right,
				'U' => Vector2.Up,
				'L' => Vector2.Left,
				'D' => Vector2.Down,
				_ => throw new Exception(),
			};

			for (int i = 0; i < dist; i++)
			{
				head += dir;
				tail = Follow(head, tail);
				positions.Add(tail);
			}
		}

		return positions.Count;
	}

	[Part(2)]
	public object Part2(string input)
	{
		var rope = new Vector2[10];
		var positions = new HashSet<Vector2> { Vector2.Zero };

		foreach (var (d, dist) in input.Lines().Extract<(char, int)>(@"(.) (\d+)"))
		{
			var dir = d switch
			{
				'R' => Vector2.Right,
				'U' => Vector2.Up,
				'L' => Vector2.Left,
				'D' => Vector2.Down,
				_ => throw new Exception(),
			};

			for (int i = 0; i < dist; i++)
			{
				rope[0] += dir;
				for (int t = 1; t < rope.Length; t++)
				{
					rope[t] = Follow(rope[t - 1], rope[t]);
				}
				positions.Add(rope[9]);
			}
		}

		return positions.Count;
	}

	private Vector2 Follow(Vector2 head, Vector2 tail)
	{
		if (tail.Distance(head) > 1.5d)
		{
			var primary = head.NeighborsVonNeumann().Where(n => tail.Distance(n) < 1.5d).OrderBy(n => tail.Distance(n));
			if (primary.Any())
			{
				tail = primary.First();
			}
			else
			{
				tail = head.NeighborsMoore().Where(n => tail.Distance(n) < 1.5d).OrderBy(n => tail.Distance(n)).First();
			}
		}
		return tail;
	}
}
