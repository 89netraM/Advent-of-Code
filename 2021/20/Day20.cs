using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;

namespace AoC.Year2021
{
	[Day(20)]
	public class Day20
	{
		[Part(1)]
		public object Part1(string input)
		{
			var split = input.Split("\n\n");
			var enhancement = split[0];
			var image = split[1].Lines().SelectMany((l, y) => l.Select((c, x) => (new Vector2(x, y), c))).ToDictionary(p => p.Item1, p => p.c == '#');
			var image2 = new Dictionary<Vector2, bool>();

			for (int i = 0; i < 2; i++)
			{
				var padValue = i % 2 == 1;

				var min = image.Keys.Aggregate((a, b) => a.MinParts(b));
				var max = image.Keys.Aggregate((a, b) => a.MaxParts(b));
				for (long y = min.Y - 1; y <= max.Y + 1; y++)
				{
					for (long x = min.X - 1; x <= max.X + 1; x++)
					{
						int index = 0;
						for (long ny = y - 1; ny <= y + 1; ny++)
						{
							for (long nx = x - 1; nx <= x + 1; nx++)
							{
								index = (index << 1) | ((image.TryGetValue(new Vector2(nx, ny), out bool b) ? b : padValue) ? 0b1 : 0b0);
							}
						}
						image2.Add(new Vector2(x, y), enhancement[index] == '#');
					}
				}

				(image, image2) = (image2, image);
				image2.Clear();
			}

			return image.Values.Count(Id);
		}
		
		[Part(2)]
		public object Part2(string input)
		{
			var split = input.Split("\n\n");
			var enhancement = split[0];
			var image = split[1].Lines().SelectMany((l, y) => l.Select((c, x) => (new Vector2(x, y), c))).ToDictionary(p => p.Item1, p => p.c == '#');
			var image2 = new Dictionary<Vector2, bool>();

			for (int i = 0; i < 50; i++)
			{
				var padValue = i % 2 == 1;

				var min = image.Keys.Aggregate((a, b) => a.MinParts(b));
				var max = image.Keys.Aggregate((a, b) => a.MaxParts(b));
				for (long y = min.Y - 1; y <= max.Y + 1; y++)
				{
					for (long x = min.X - 1; x <= max.X + 1; x++)
					{
						int index = 0;
						for (long ny = y - 1; ny <= y + 1; ny++)
						{
							for (long nx = x - 1; nx <= x + 1; nx++)
							{
								index = (index << 1) | ((image.TryGetValue(new Vector2(nx, ny), out bool b) ? b : padValue) ? 0b1 : 0b0);
							}
						}
						image2.Add(new Vector2(x, y), enhancement[index] == '#');
					}
				}

				(image, image2) = (image2, image);
				image2.Clear();
			}

			return image.Values.Count(Id);
		}
	}
}
