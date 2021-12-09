using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using Priority_Queue;

namespace AoC.Year2021
{
	[Day(9)]
	public class Day9
	{
		[Part(1)]
		public object Part1(string input)
		{
			long sum = 0;
			Dictionary<Vector2, long> map = input.Lines().SelectMany(static (l, y) => l.Select((c, x) => (new Vector2(x, y), c))).ToDictionary(p => p.Item1, p => (long)(p.c - '0'));
			foreach (var kvp in map)
			{
				bool isLow = true;
				foreach (var n in kvp.Key.NeighborsVonNeumann())
				{
					if (map.TryGetValue(n, out var val) && val <= kvp.Value)
					{
						isLow = false;
						break;
					}
				}
				if (isLow)
				{
					sum += kvp.Value + 1;
				}
			}
			return sum;
		}

		[Part(2)]
		public object Part2(string input)
		{
			Dictionary<Vector2, long> map = input.Lines().SelectMany(static (l, y) => l.Select((c, x) => (new Vector2(x, y), c))).ToDictionary(p => p.Item1, p => (long)(p.c - '0'));
			List<Vector2> lowPoints = new List<Vector2>();
			foreach (var kvp in map)
			{
				bool isLow = true;
				foreach (var n in kvp.Key.NeighborsVonNeumann())
				{
					if (map.TryGetValue(n, out var val) && val <= kvp.Value)
					{
						isLow = false;
						break;
					}
				}
				if (isLow)
				{
					lowPoints.Add(kvp.Key);
				}
			}

			IPriorityQueue<long, long> sizes = new SimplePriorityQueue<long, long>();
			foreach (var lowPoint in lowPoints)
			{
				long size = 1;
				BFS.Search(
					lowPoint,
					c => c.NeighborsVonNeumann().Where(n => map.TryGetValue(n, out var val) && val != 9),
					c => map[c] == 9,
					out _,
					c => size++
				);
				sizes.Enqueue(size, -size);
			}

			return sizes.Dequeue() * sizes.Dequeue() * sizes.Dequeue();
		}
	}
}
