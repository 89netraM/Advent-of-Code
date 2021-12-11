using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2021
{
	[Day(11)]
	public class Day11
	{
		[Part(1)]
		public object Part1(string input)
		{
			long flashes = 0;
			var map = input.Lines().SelectMany((l, y) => l.Select((c, x) => (coord: new Vector2(x, y), val: (long)(c - '0')))).ToDictionary(p => p.coord, p => p.val);
			var flashed = new HashSet<Vector2>();
			var toFlash = new HashSet<Vector2>();
			var toFlashNext = new HashSet<Vector2>();

			for (int i = 0; i < 100; i++)
			{
				foreach (var coord in map.Keys)
				{
					if (map[coord] == 9)
					{
						toFlashNext.Add(coord);
					}
					else
					{
						map[coord] = map[coord] + 1;
					}
				}
				(toFlash, toFlashNext) = (toFlashNext, toFlash);
				toFlashNext.Clear();
				while (toFlash.Count > 0)
				{
					foreach (var c in toFlash) { flashed.Add(c); }
					foreach (var coord in toFlash)
					{
						foreach (var n in coord.NeighborsMoore())
						{
							if (map.ContainsKey(n) && !flashed.Contains(n))
							{
								if (map[n] == 9)
								{
									toFlashNext.Add(n);
								}
								else
								{
									map[n] = map[n] + 1;
								}
							}
						}
					}
					(toFlash, toFlashNext) = (toFlashNext, toFlash);
					toFlashNext.Clear();
				}
				flashes += flashed.Count;
				foreach (var coord in flashed)
				{
					map[coord] = 0;
				}
				flashed.Clear();
			}

			return flashes;
		}

		[Part(2)]
		public object Part2(string input)
		{
			long flashes = 0;
			var map = input.Lines().SelectMany((l, y) => l.Select((c, x) => (coord: new Vector2(x, y), val: (long)(c - '0')))).ToDictionary(p => p.coord, p => p.val);
			var flashed = new HashSet<Vector2>();
			var toFlash = new HashSet<Vector2>();
			var toFlashNext = new HashSet<Vector2>();

			for (int i = 0; true; i++)
			{
				foreach (var coord in map.Keys)
				{
					if (map[coord] == 9)
					{
						toFlashNext.Add(coord);
					}
					else
					{
						map[coord] = map[coord] + 1;
					}
				}
				(toFlash, toFlashNext) = (toFlashNext, toFlash);
				toFlashNext.Clear();
				while (toFlash.Count > 0)
				{
					foreach (var c in toFlash) { flashed.Add(c); }
					foreach (var coord in toFlash)
					{
						foreach (var n in coord.NeighborsMoore())
						{
							if (map.ContainsKey(n) && !flashed.Contains(n))
							{
								if (map[n] == 9)
								{
									toFlashNext.Add(n);
								}
								else
								{
									map[n] = map[n] + 1;
								}
							}
						}
					}
					(toFlash, toFlashNext) = (toFlashNext, toFlash);
					toFlashNext.Clear();
				}
				flashes += flashed.Count;
				foreach (var coord in flashed)
				{
					map[coord] = 0;
				}
				if (flashed.Count == map.Count)
				{
					return i + 1;
				}
				flashed.Clear();
			}
		}
	}
}
