using System;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2021
{
	[Day(13)]
	public class Day13
	{
		[Part(1)]
		public object Part1(string input)
		{
			var s = input.Split("\n\n");
			var dots = s[0].Lines().Extract<Vector2>(@"(\d+),(\d+)").ToHashSet();
			var folds = s[1].Lines().Extract<(char dir, long pos)>(@"fold along (x|y)=(\d+)").ToArray();

			var fold = folds[0];

			if (fold.dir == 'x')
			{
				foreach (var dot in dots.ToArray())
				{
					if (dot.X > fold.pos)
					{
						dots.Remove(dot);
						dots.Add(new Vector2(fold.pos - (dot.X - fold.pos), dot.Y));
					}
				}
			}
			else
			{
				foreach (var dot in dots.ToArray())
				{
					if (dot.Y > fold.pos)
					{
						dots.Remove(dot);
						dots.Add(new Vector2(dot.X, fold.pos - (dot.Y - fold.pos)));
					}
				}
			}

			return dots.Count;
		}

		[Part(2)]
		public object Part2(string input)
		{
			var s = input.Split("\n\n");
			var dots = s[0].Lines().Extract<Vector2>(@"(\d+),(\d+)").ToHashSet();
			var folds = s[1].Lines().Extract<(char dir, long pos)>(@"fold along (x|y)=(\d+)").ToArray();

			foreach (var fold in folds)
			{
				if (fold.dir == 'x')
				{
					foreach (var dot in dots.ToArray())
					{
						if (dot.X > fold.pos)
						{
							dots.Remove(dot);
							dots.Add(new Vector2(fold.pos - (dot.X - fold.pos), dot.Y));
						}
					}
				}
				else
				{
					foreach (var dot in dots.ToArray())
					{
						if (dot.Y > fold.pos)
						{
							dots.Remove(dot);
							dots.Add(new Vector2(dot.X, fold.pos - (dot.Y - fold.pos)));
						}
					}
				}
			}

			var maxX = dots.Max(d => d.X) + 1;
			var maxY = dots.Max(d => d.Y) + 1;
			for (long y = -1; y <= maxY; y++)
			{
				for (long x = -1; x <= maxX; x++)
				{
					Console.Write(dots.Contains(new Vector2(x, y)) ? "#" : ".");
				}
				Console.WriteLine();
			}

			return null;
		}
	}
}
