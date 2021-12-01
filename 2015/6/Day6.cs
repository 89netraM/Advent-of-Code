using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2015
{
	[Day(6)]
	public class Day6
	{
		Func<string, (long, long, long, long)?> turnOn = Parsing.PatternParser<long, long, long, long>(@"turn on (?<x1>\d+),(?<y1>\d+) through (?<x2>\d+),(?<y2>\d+)");
		Func<string, (long, long, long, long)?> turnOff = Parsing.PatternParser<long, long, long, long>(@"turn off (?<x1>\d+),(?<y1>\d+) through (?<x2>\d+),(?<y2>\d+)");
		Func<string, (long, long, long, long)?> toggle = Parsing.PatternParser<long, long, long, long>(@"toggle (?<x1>\d+),(?<y1>\d+) through (?<x2>\d+),(?<y2>\d+)");

		[Part(1)]
		public object Part1(string input)
		{
			var on = new HashSet<Vector2>();
			Vector2 min, max;
			Action<Vector2> exec;
			foreach (var line in input.Lines())
			{
				if (turnOn(line) is (long x1, long y1, long x2, long y2))
				{
					min = new Vector2(x1, y1);
					max = new Vector2(x2, y2);
					exec = v => on.Add(v);
				}
				else if (turnOff(line) is (long x12, long y12, long x22, long y22))
				{
					min = new Vector2(x12, y12);
					max = new Vector2(x22, y22);
					exec = v => on.Remove(v);
				}
				else if (toggle(line) is (long x13, long y13, long x23, long y23))
				{
					min = new Vector2(x13, y13);
					max = new Vector2(x23, y23);
					exec = v => { if (!on.Add(v)) { on.Remove(v); } };
				}
				else
				{
					throw new Exception("Invalid input");
				}

				for (long x = min.X; x <= max.X; x++)
				{
					for (long y = min.Y; y <= max.Y; y++)
					{
						exec(new Vector2(x, y));
					}
				}
			}
			return on.Count;
		}

		[Part(2)]
		public object Part2(string input)
		{
			var on = new Dictionary<Vector2, long>();
			Vector2 min, max;
			Action<Vector2> exec;
			foreach (var line in input.Lines())
			{
				if (turnOn(line) is (long x1, long y1, long x2, long y2))
				{
					min = new Vector2(x1, y1);
					max = new Vector2(x2, y2);
					exec = v => { if (on.ContainsKey(v)) { on[v]++; } else { on[v] = 1L; } };
				}
				else if (turnOff(line) is (long x12, long y12, long x22, long y22))
				{
					min = new Vector2(x12, y12);
					max = new Vector2(x22, y22);
					exec = v => { if (on.ContainsKey(v)) { on[v] = Math.Max(0L, on[v] - 1L); } };
				}
				else if (toggle(line) is (long x13, long y13, long x23, long y23))
				{
					min = new Vector2(x13, y13);
					max = new Vector2(x23, y23);
					exec = v => { if (on.ContainsKey(v)) { on[v] += 2; } else { on[v] = 2L; } };
				}
				else
				{
					throw new Exception("Invalid input");
				}

				for (long x = min.X; x <= max.X; x++)
				{
					for (long y = min.Y; y <= max.Y; y++)
					{
						exec(new Vector2(x, y));
					}
				}
			}
			return on.Values.Sum();
		}
	}
}
