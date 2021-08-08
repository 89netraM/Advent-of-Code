using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(3)]
	public class Day3
	{
		[Part(1)]
		public object Part1(string input)
		{
			int inputInt = Int32.Parse(input);
			Vector2? position = PositionFromValue(inputInt);
			return Vector.Zero<Vector2>().ManhattanDistance(position ?? Vector.Zero<Vector2>());
		}

		private static Vector2? PositionFromValue(int targetValue)
		{
			for (int i = 0; true; i++)
			{
				int value = (int)Math.Pow(1 + 2 * i, 2);
				if (targetValue <= value)
				{
					int x = i;
					int y = i;
					int ringSide = 1 + i * 2;
					for (int j = 1; j < ringSide; j++)
					{
						if (targetValue == value)
						{
							return new Vector2(x, y);
						}
						x--;
						value--;
					}
					for (int j = 1; j < ringSide; j++)
					{
						if (targetValue == value)
						{
							return new Vector2(x, y);
						}
						y--;
						value--;
					}
					for (int j = 1; j < ringSide; j++)
					{
						if (targetValue == value)
						{
							return new Vector2(x, y);
						}
						x++;
						value--;
					}
					for (int j = 1; j < ringSide; j++)
					{
						if (targetValue == value)
						{
							return new Vector2(x, y);
						}
						y++;
						value--;
					}
					return null;
				}
			}
		}

		[Part(2)]
		public object Part2(string input)
		{
			int inputInt = Int32.Parse(input);
			IDictionary<Vector2, int> positions = new Dictionary<Vector2, int>
			{
				[Vector.Zero<Vector2>()] = 1,
			};
			Vector2 position = new Vector2(1, 0);
			while (true)
			{
				int value = position.NeighborsMoore().Sum(n => positions.TryGetValue(n, out int v) ? v : 0);
				positions.Add(position, value);
				if (value > inputInt)
				{
					return value;
				}

				if (Math.Abs(position.X) > Math.Abs(position.Y))
				{
					position = new Vector2(position.X, position.Y + (position.X > 0 ? 1 : -1));
				}
				else if (position.Y > 0 && position.X == -position.Y)
				{
					position = new Vector2(position.X, position.Y - 1);
				}
				else
				{
					position = new Vector2(position.X + (position.Y > 0 ? -1 : 1), position.Y);
				}
			}
		}
	}
}
