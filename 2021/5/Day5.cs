using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2021
{
	[Day(5)]
	public class Day5
	{
		[Part(1)]
		public object Part1(string input)
		{
			Dictionary<Vector2, long> map = new Dictionary<Vector2, long>();
			foreach (var (from, to) in input.Lines().Extract<(Vector2, Vector2)>(@"((\d+),(\d+)) -> ((\d+),(\d+))"))
			{
				if (from.X == to.X)
				{
					var f = from.MinParts(to);
					var t = from.MaxParts(to);
					for (long y = f.Y; y <= t.Y; y++)
					{
						var pos = new Vector2(f.X, y);
						if (map.ContainsKey(pos)) { map[pos]++; } else { map.Add(pos, 1); }
					}
				}
				else if (from.Y == to.Y)
				{
					var f = from.MinParts(to);
					var t = from.MaxParts(to);
					for (long x = f.X; x <= t.X; x++)
					{
						var pos = new Vector2(x, f.Y);
						if (map.ContainsKey(pos)) { map[pos]++; } else { map.Add(pos, 1); }
					}
				}
			}
			return map.Count(kvp => kvp.Value >= 2);
		}

		[Part(2)]
		public object Part2(string input)
		{
			Dictionary<Vector2, long> map = new Dictionary<Vector2, long>();
			foreach (var (from, to) in input.Lines().Extract<(Vector2, Vector2)>(@"((\d+),(\d+)) -> ((\d+),(\d+))"))
			{
				Vector2 step = to - from;
				step = new Vector2(step.X == 0 ? 0 : 1 * Math.Sign(step.X), step.Y == 0 ? 0 : 1 * Math.Sign(step.Y));
				for (Vector2 pos = from; pos != to; pos += step)
				{
					if (map.ContainsKey(from)) { map[from]++; } else { map.Add(from, 1); }
				}
				if (map.ContainsKey(from)) { map[from]++; } else { map.Add(from, 1); }
			}
			return map.Count(kvp => kvp.Value >= 2);
		}
	}
}
