using System.Linq;
using AoC.Library;

namespace AoC.Year2022;

[Day(8)]
public class Day8
{
	[Part(1)]
	public object Part1(string input)
	{
		var map = input.ToMapLong().ToDictionary(kvp => kvp.Key, kvp => (h: kvp.Value, v: Visible.None));
		var min = Vector2.Zero;
		var max = map.Keys.Aggregate((a, b) => a.MaxParts(b));

		for (long x = min.X; x <= max.X; x++)
		{
			long height = -1;
			for (long y = min.Y; y <= max.Y; y++)
			{
				var (h, v) = map[new(x, y)];
				if (h > height)
				{
					height = h;
					map[new(x, y)] = (h, v | Visible.North);
				}
			}
		}
		for (long x = min.X; x <= max.X; x++)
		{
			long height = -1;
			for (long y = max.Y; y >= min.Y; y--)
			{
				var (h, v) = map[new(x, y)];
				if (h > height)
				{
					height = h;
					map[new(x, y)] = (h, v | Visible.South);
				}
			}
		}
		for (long y = min.Y; y <= max.Y; y++)
		{
			long height = -1;
			for (long x = min.X; x <= max.X; x++)
			{
				var (h, v) = map[new(x, y)];
				if (h > height)
				{
					height = h;
					map[new(x, y)] = (h, v | Visible.West);
				}
			}
		}
		for (long y = min.Y; y <= max.Y; y++)
		{
			long height = -1;
			for (long x = max.X; x >= min.X; x--)
			{
				var (h, v) = map[new(x, y)];
				if (h > height)
				{
					height = h;
					map[new(x, y)] = (h, v | Visible.East);
				}
			}
		}

		return map.Count(kvp => kvp.Value.v != Visible.None);
	}

	[Part(2)]
	public object Part2(string input)
	{
		var map = input.ToMapLong();
		var min = Vector2.Zero;
		var max = map.Keys.Aggregate((a, b) => a.MaxParts(b));

		var maximum = 0L;
		var maxPos = new Vector2(-1, -1);

		for (long y = min.Y; y <= max.Y; y++)
		{
			for (long x = min.X; x <= max.Y; x++)
			{
				var score = 1L;
				var pos = new Vector2(x, y);
				var height = map[pos];

				var n = pos + Vector2.Up;
				for (long i = 1; true; i++)
				{
					if (!map.TryGetValue(n, out var h))
					{
						score *= i - 1;
						break;
					}
					if (h >= height)
					{
						score *= i;
						break;
					}
					n += Vector2.Up;
				}
				n = pos + Vector2.Left;
				for (long i = 1; true; i++)
				{
					if (!map.TryGetValue(n, out var h))
					{
						score *= i - 1;
						break;
					}
					if (h >= height)
					{
						score *= i;
						break;
					}
					n += Vector2.Left;
				}
				n = pos + Vector2.Right;
				for (long i = 1; true; i++)
				{
					if (!map.TryGetValue(n, out var h))
					{
						score *= i - 1;
						break;
					}
					if (h >= height)
					{
						score *= i;
						break;
					}
					n += Vector2.Right;
				}
				n = pos + Vector2.Down;
				for (long i = 1; true; i++)
				{
					if (!map.TryGetValue(n, out var h))
					{
						score *= i - 1;
						break;
					}
					if (h >= height)
					{
						score *= i;
						break;
					}
					n += Vector2.Down;
				}

				if (score > maximum)
				{
					maximum = score;
					maxPos = pos;
				}
			}
		}

		return maximum;
	}

	enum Visible
	{
		None = 0b0000,
		North = 0b0001,
		East = 0b0010,
		West = 0b0100,
		South = 0b1000,
	}
}
