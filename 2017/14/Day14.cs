using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(14)]
	public class Day14
	{
		[Part(1)]
		public object Part1(string input)
		{
			HashSet<Vector2> used = new HashSet<Vector2>();
			for (int y = 0; y < 128; y++)
			{
				string bits = String.Concat(KnotHash.Calculate($"{input}-{y}").Select(static s => Convert.ToString(s, 2).PadLeft(8, '0')));
				for (int x = 0; x < 128; x++)
				{
					if (bits[x] == '1')
					{
						used.Add(new Vector2(x, y));
					}
				}
			}

			return used.Count;
		}

		[Part(2)]
		public object Part2(string input)
		{
			HashSet<Vector2> used = new HashSet<Vector2>();
			for (int y = 0; y < 128; y++)
			{
				string bits = String.Concat(KnotHash.Calculate($"{input}-{y}").Select(static s => Convert.ToString(s, 2).PadLeft(8, '0')));
				for (int x = 0; x < 128; x++)
				{
					if (bits[x] == '1')
					{
						used.Add(new Vector2(x, y));
					}
				}
			}

			int regions = 0;
			while (used.Count > 0)
			{
				Vector2 current = used.First();
				Queue<Vector2> queue = new Queue<Vector2>();
				queue.Enqueue(current);
				while (queue.Count > 0)
				{
					Vector2 currentPos = queue.Dequeue();
					used.Remove(currentPos);
					foreach (Vector2 nextPos in currentPos.NeighborsVonNeumann())
					{
						if (used.Contains(nextPos))
						{
							queue.Enqueue(nextPos);
						}
					}
				}
				regions++;
			}

			return regions;
		}
	}
}
