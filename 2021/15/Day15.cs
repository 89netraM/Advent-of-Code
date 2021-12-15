using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2021
{
	[Day(15)]
	public class Day15
	{
		[Part(1)]
		public object Part1(string input)
		{
			var map = input.Lines().SelectMany((l, y) => l.Select((r, x) => (x, y, r))).ToDictionary(p => new Vector2(p.x, p.y), p => (long)(p.r - '0'));
			var lowerRight = map.Keys.Aggregate(Vector2.Zero, (a, c) => a.MaxParts(c));
			BFS.Search(
				Vector2.Zero,
				c => c.NeighborsVonNeumann().Where(n => map.ContainsKey(n)).Select(n => (n, map[n])),
				c => c == lowerRight,
				out var path
			);
			return path.Last().Item2;
		}

		[Part(2)]
		public object Part2(string input)
		{
			var map = input.Lines().SelectMany((l, y) => l.Select((r, x) => (x, y, r))).ToDictionary(p => new Vector2(p.x, p.y), p => (long)(p.r - '0'));
			var lowerRight = map.Keys.Aggregate(Vector2.Zero, (a, c) => a.MaxParts(c));
			var completeMap = new Dictionary<Vector2, long>();
			for (long y = 0; y < 5; y++)
			{
				for (long x = 0; x < 5; x++)
				{
					foreach (var kvp in map)
					{
						var pos = kvp.Key + new Vector2(x * (lowerRight.X + 1), y * (lowerRight.Y + 1));
						var value = MathM.Mod(kvp.Value + x + y - 1, 9) + 1;
						completeMap[pos] = value;
					}
				}
			}
			var completeLowerRight = completeMap.Keys.Aggregate(Vector2.Zero, (a, c) => a.MaxParts(c));
			BFS.Search(
				Vector2.Zero,
				c => c.NeighborsVonNeumann().Where(n => completeMap.ContainsKey(n)).Select(n => (n, completeMap[n])),
				c => c == completeLowerRight,
				out var path
			);
			return path.Last().Item2;
		}
	}
}
