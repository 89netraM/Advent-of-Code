using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using AoC.Library;

namespace AoC.Year2021
{
	[Day(5)]
	public class Day5
	{
		[Part(1)]
		public object Part1(string input)
		{
			Regex regex = new Regex(@"(\d+),(\d+) -> (\d+),(\d+)");
			Dictionary<Vector2, long> map = new Dictionary<Vector2, long>();
			foreach (var line in input.Lines())
			{
				var m = regex.Match(line);
				if (m.Success)
				{
					Vector2 from = new Vector2(long.Parse(m.Groups[1].Value), long.Parse(m.Groups[2].Value));
					Vector2 to = new Vector2(long.Parse(m.Groups[3].Value), long.Parse(m.Groups[4].Value));
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
				else
				{
					Console.WriteLine($"Unable to parse line: {line}");
				}
			}
			return map.Count(kvp => kvp.Value >= 2);
		}

		[Part(2)]
		public object Part2(string input)
		{
			Regex regex = new Regex(@"(\d+),(\d+) -> (\d+),(\d+)");
			Dictionary<Vector2, long> map = new Dictionary<Vector2, long>();
			foreach (var line in input.Lines())
			{
				var m = regex.Match(line);
				if (m.Success)
				{
					Vector2 from = new Vector2(long.Parse(m.Groups[1].Value), long.Parse(m.Groups[2].Value));
					Vector2 to = new Vector2(long.Parse(m.Groups[3].Value), long.Parse(m.Groups[4].Value));

					Vector2 step = to - from;
					step = new Vector2(step.X == 0 ? 0 : 1 * Math.Sign(step.X), step.Y == 0 ? 0 : 1 * Math.Sign(step.Y));
					while (from != to)
					{
						if (map.ContainsKey(from)) { map[from]++; } else { map.Add(from, 1); }
						from += step;
					}
					if (map.ContainsKey(from)) { map[from]++; } else { map.Add(from, 1); }
				}
				else
				{
					Console.WriteLine($"Unable to parse line: {line}");
				}
			}
			return map.Count(kvp => kvp.Value >= 2);
		}
	}
}
