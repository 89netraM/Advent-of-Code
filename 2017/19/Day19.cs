using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(19)]
	public class Day19
	{
		[Part(1)]
		public object Part1(string input)
		{
			string[] map = input.Lines();
			HashSet<char> letters = input.Where(Char.IsLetter).ToHashSet();
			var start = map[0].IndexOf('|');
			Vector2 pos = new Vector2(start, 0);
			Vector2 dir = Vector2.Down;
			var path = new List<char>();

			while (letters.Count > 0)
			{
				char posChar = map[(int)pos.Y][(int)pos.X];
				if (Char.IsLetter(posChar))
				{
					path.Add(posChar);
					letters.Remove(posChar);
				}
				else if (posChar == '+')
				{
					if (dir == Vector2.Down || dir == Vector2.Up)
					{
						if (map[(int)pos.Y][(int)pos.X - 1] == ' ')
						{
							dir = Vector2.Right;
						}
						else
						{
							dir = Vector2.Left;
						}
					}
					else
					{
						if (map[(int)pos.Y - 1][(int)pos.X] == ' ')
						{
							dir = Vector2.Down;
						}
						else
						{
							dir = Vector2.Up;
						}
					}
				}
				pos += dir;
			}

			return String.Concat(path);
		}

		[Part(2)]
		public object Part2(string input)
		{
			string[] map = input.Lines();
			HashSet<char> letters = input.Where(Char.IsLetter).ToHashSet();
			var start = map[0].IndexOf('|');
			Vector2 pos = new Vector2(start, 0);
			Vector2 dir = Vector2.Down;
			var path = new List<char>();
			long steps = 0;

			while (letters.Count > 0)
			{
				char posChar = map[(int)pos.Y][(int)pos.X];
				if (Char.IsLetter(posChar))
				{
					path.Add(posChar);
					letters.Remove(posChar);
				}
				else if (posChar == '+')
				{
					if (dir == Vector2.Down || dir == Vector2.Up)
					{
						if (map[(int)pos.Y][(int)pos.X - 1] == ' ')
						{
							dir = Vector2.Right;
						}
						else
						{
							dir = Vector2.Left;
						}
					}
					else
					{
						if (map[(int)pos.Y - 1][(int)pos.X] == ' ')
						{
							dir = Vector2.Down;
						}
						else
						{
							dir = Vector2.Up;
						}
					}
				}
				pos += dir;
				steps++;
			}

			return steps;
		}
	}
}
