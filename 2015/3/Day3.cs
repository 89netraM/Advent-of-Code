using System;
using System.Collections.Generic;
using AoC.Library;

namespace AoC.Year2015
{
	[Day(3)]
	public class Day3
	{
		[Part(1)]
		public object Part1(string input)
		{
			HashSet<Vector2> poses = new HashSet<Vector2> { Vector2.Zero };
			Vector2 pos = Vector2.Zero;
			poses.Add(pos);
			long multiple = 0;
			foreach (char c in input.Trim())
			{
				pos = pos + c switch {
					'^' => Vector2.Up,
					'v' => Vector2.Down,
					'<' => Vector2.Left,
					'>' => Vector2.Right,
					_ => throw new ArgumentException()
				};
				if (poses.Add(pos))
					multiple++;
			}
			return poses.Count;
		}

		[Part(2)]
		public object Part2(string input)
		{
			HashSet<Vector2> poses = new HashSet<Vector2> { Vector2.Zero };
			Vector2 posS = Vector2.Zero;
			Vector2 posR = Vector2.Zero;
			input = input.Trim();
			for (int i = 0; i < input.Length / 2; i++)
			{
				posS = posS + input[i * 2] switch {
					'^' => Vector2.Up,
					'v' => Vector2.Down,
					'<' => Vector2.Left,
					'>' => Vector2.Right,
					_ => throw new ArgumentException()
				};
				poses.Add(posS);
				posR = posR + input[i * 2 + 1] switch {
					'^' => Vector2.Up,
					'v' => Vector2.Down,
					'<' => Vector2.Left,
					'>' => Vector2.Right,
					_ => throw new ArgumentException()
				};
				poses.Add(posR);
			}
			return poses.Count;
		}
	}
}
