using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;

namespace AoC.Year2016;

[Day(13)]
public class Day13
{
	[Part(1)]
	public object Part1(string input)
	{
		var floor = new Floor(Int64.Parse(input));
		var start = new Vector2(1, 1);
		var target = new Vector2(31, 39);

		BFS.Search(
			start,
			pos => pos.NeighborsVonNeumann().Where(floor.IsOpen),
			pos => pos == target,
			out var path);

		return path.Count();
	}

	[Part(2)]
	public object Part2(string input)
	{
		var floor = new Floor(Int64.Parse(input));
		var start = new Vector2(1, 1);
		var target = new Vector2(31, 39);

		var poses = new HashSet<Vector2>();
		poses.Add(start);
		try
		{
			BFS.Search(
				start,
				pos => pos.NeighborsVonNeumann().Where(floor.IsOpen).Select(p => (p, 1L)),
				pos => false,
				out _,
				(pos, dist) =>
				{
					if (dist > 50)
					{
						throw new Exception();
					}
					poses.Add(pos);
				});
		}
		catch { }

		return poses.Count;
	}

	private class Floor
	{
		private readonly long number;
		public Func<Vector2, bool> IsOpen { get; }
		public Floor(long number)
		{
			this.number = number;
			IsOpen = Memoize<Vector2, bool>(isOpen);
		}

		private bool isOpen(Vector2 pos)
		{
			var (x, y) = pos;

			if (x < 0 || y < 0)
			{
				return false;
			}

			var s = x * x + 3 * x + 2 * x * y + y + y * y + number;
			return CountBits(s) % 2 == 0;

			static long CountBits(long l)
			{
				long bits = 0;
				for (int i = 0; i < 64; i++)
				{
					bits += l & 1;
					l >>= 1;
				}
				return bits;
			}
		}
	}
}
