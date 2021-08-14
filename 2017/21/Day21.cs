using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(21)]
	public class Day21
	{
		[Part(1)]
		public object Part1(string input)
		{
			var lines = input.Lines();
			var twoRules = lines[..6]
				.Select(Parsing.PatternParser<string, string>(@"(.*?) => (.*)"))
				.Select(static p => (RotateAndFlip2(p?.Item1).ToArray(), p?.Item2.Split('/')))
				.ToArray();
			var threeRules = lines[6..]
				.Select(Parsing.PatternParser<string, string>(@"(.*?) => (.*)"))
				.Select(static p => (RotateAndFlip3(p?.Item1).ToArray(), p?.Item2.Split('/')))
				.ToArray();

			HashSet<Vector2> art = new HashSet<Vector2>
			{
				new Vector2(1, 0),

				new Vector2(2, 1),

				new Vector2(0, 2),
				new Vector2(1, 2),
				new Vector2(2, 2),
			};
			HashSet<Vector2> artOut = new HashSet<Vector2>();

			for (int i = 0; i < 5; i++)
			{
				Vector2 max = art.Aggregate(Vector2.Zero, static (acc, c) => acc.MaxParts(c));
				bool isTwo = (max.X + 1) % 2 == 0 || (max.Y + 1) % 2 == 0;
				long mult = isTwo ? 2 : 3;
				var rules = isTwo ? twoRules : threeRules;
				for (long x = 0; x <= max.X / mult; x++)
				{
					for (long y = 0; y <= max.Y / mult; y++)
					{
						Update(art, artOut, x, y, mult, rules);
					}
				}

				(art, artOut) = (artOut, art);
				artOut.Clear();
			}

			return art.Count;
		}

		private static IEnumerable<string[]> RotateAndFlip2(string s)
		{
			string[] arr = s.Split('/');
			yield return arr;

			// Rotate arr 90deg
			string[] rot1 = new[] { $"{arr[1][0]}{arr[0][0]}", $"{arr[1][1]}{arr[0][1]}" };
			yield return rot1;
			string[] rot2 = new[] { $"{rot1[1][0]}{rot1[0][0]}", $"{rot1[1][1]}{rot1[0][1]}" };
			yield return rot2;
			string[] rot3 = new[] { $"{rot2[1][0]}{rot2[0][0]}", $"{rot2[1][1]}{rot2[0][1]}" };
			yield return rot3;

			// Flip arr horizontal
			yield return new[] { arr[1], arr[0] };
			yield return new[] { rot1[1], rot1[0] };
			yield return new[] { rot2[1], rot2[0] };
			yield return new[] { rot3[1], rot3[0] };

			// Flip arr vertical
			yield return new[] { $"{arr[0][1]}{arr[0][0]}", $"{arr[1][1]}{arr[1][0]}" };
			yield return new[] { $"{rot1[0][1]}{rot1[0][0]}", $"{rot1[1][1]}{rot1[1][0]}" };
			yield return new[] { $"{rot2[0][1]}{rot2[0][0]}", $"{rot2[1][1]}{rot2[1][0]}" };
			yield return new[] { $"{rot3[0][1]}{rot3[0][0]}", $"{rot3[1][1]}{rot3[1][0]}" };
		}

		private static IEnumerable<string[]> RotateAndFlip3(string s)
		{
			string[] arr = s.Split('/');
			yield return arr;

			// Rotate arr 90deg
			string[] rot1 = new[] { $"{arr[2][0]}{arr[1][0]}{arr[0][0]}", $"{arr[2][1]}{arr[1][1]}{arr[0][1]}", $"{arr[2][2]}{arr[1][2]}{arr[0][2]}" };
			yield return rot1;
			string[] rot2 = new[] { $"{rot1[2][0]}{rot1[1][0]}{rot1[0][0]}", $"{rot1[2][1]}{rot1[1][1]}{rot1[0][1]}", $"{rot1[2][2]}{rot1[1][2]}{rot1[0][2]}" };
			yield return rot2;
			string[] rot3 = new[] { $"{rot2[2][0]}{rot2[1][0]}{rot2[0][0]}", $"{rot2[2][1]}{rot2[1][1]}{rot2[0][1]}", $"{rot2[2][2]}{rot2[1][2]}{rot2[0][2]}" };
			yield return rot3;

			// Flip arr horizontal
			yield return new[] { arr[2], arr[1], arr[0] };
			yield return new[] { rot1[2], rot1[1], rot1[0] };
			yield return new[] { rot2[2], rot2[1], rot2[0] };
			yield return new[] { rot3[2], rot3[1], rot3[0] };

			// Flip arr vertical
			yield return new[] { $"{arr[0][2]}{arr[0][1]}{arr[0][0]}", $"{arr[1][2]}{arr[1][1]}{arr[1][0]}", $"{arr[2][2]}{arr[2][1]}{arr[2][0]}" };
			yield return new[] { $"{rot1[0][2]}{rot1[0][1]}{rot1[0][0]}", $"{rot1[1][2]}{rot1[1][1]}{rot1[1][0]}", $"{rot1[2][2]}{rot1[2][1]}{rot1[2][0]}" };
			yield return new[] { $"{rot2[0][2]}{rot2[0][1]}{rot2[0][0]}", $"{rot2[1][2]}{rot2[1][1]}{rot2[1][0]}", $"{rot2[2][2]}{rot2[2][1]}{rot2[2][0]}" };
			yield return new[] { $"{rot3[0][2]}{rot3[0][1]}{rot3[0][0]}", $"{rot3[1][2]}{rot3[1][1]}{rot3[1][0]}", $"{rot3[2][2]}{rot3[2][1]}{rot3[2][0]}" };
		}

		private static void Update(HashSet<Vector2> art, HashSet<Vector2> artOut, long x, long y, long mult, (string[][], string[])[] rules)
		{
			foreach (var (patterns, output) in rules)
			{
				foreach (var pattern in patterns)
				{
					bool patternFound = true;
					for (int i = 0; i < pattern.Length; i++)
					{
						for (int j = 0; j < pattern[i].Length; j++)
						{
							if ((pattern[i][j] == '#') != art.Contains(new Vector2(x * mult + i, y * mult + j)))
							{
								patternFound = false;
								break;
							}
						}
						if (!patternFound)
						{
							break;
						}
					}
					if (patternFound)
					{
						for (int i = 0; i < output.Length; i++)
						{
							for (int j = 0; j < output[i].Length; j++)
							{
								if (output[i][j] == '#')
								{
									artOut.Add(new Vector2(x * (mult + 1) + i, y * (mult + 1) + j));
								}
							}
						}
						return;
					}
				}
			}
		}

		[Part(2)]
		public object Part2(string input)
		{
			var lines = input.Lines();
			var twoRules = lines[..6]
				.Select(Parsing.PatternParser<string, string>(@"(.*?) => (.*)"))
				.Select(static p => (RotateAndFlip2(p?.Item1).ToArray(), p?.Item2.Split('/')))
				.ToArray();
			var threeRules = lines[6..]
				.Select(Parsing.PatternParser<string, string>(@"(.*?) => (.*)"))
				.Select(static p => (RotateAndFlip3(p?.Item1).ToArray(), p?.Item2.Split('/')))
				.ToArray();

			HashSet<Vector2> art = new HashSet<Vector2>
			{
				new Vector2(1, 0),

				new Vector2(2, 1),

				new Vector2(0, 2),
				new Vector2(1, 2),
				new Vector2(2, 2),
			};
			HashSet<Vector2> artOut = new HashSet<Vector2>();

			for (int i = 0; i < 18; i++)
			{
				Vector2 max = art.Aggregate(Vector2.Zero, static (acc, c) => acc.MaxParts(c));
				bool isTwo = (max.X + 1) % 2 == 0 || (max.Y + 1) % 2 == 0;
				long mult = isTwo ? 2 : 3;
				var rules = isTwo ? twoRules : threeRules;
				for (long x = 0; x <= max.X / mult; x++)
				{
					for (long y = 0; y <= max.Y / mult; y++)
					{
						Update(art, artOut, x, y, mult, rules);
					}
				}

				(art, artOut) = (artOut, art);
				artOut.Clear();
			}

			return art.Count;
		}
	}
}
