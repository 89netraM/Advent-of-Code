using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

#nullable enable

namespace AoC.Year2021
{
	[Day(4)]
	public class Day4
	{
		[Part(1)]
		public object? Part1(string input)
		{
			var numbers = input.Split("\n\n").First().Split(",").Select(long.Parse).ToArray();
			var boards = input.Split("\n\n").Skip(1).Select(b => new BingoBoard(b)).ToList();

			foreach (var number in numbers)
			{
				foreach (var b in boards)
				{
					if (b.MarkNumber(number) is long[] row)
					{
						return b.UnmarkedNumbers() * number;
					}
				}
			}

			return null;
		}

		[Part(2)]
		public object? Part2(string input)
		{
			var numbers = input.Split("\n\n").First().Split(",").Select(long.Parse).ToArray();
			var boards = input.Split("\n\n").Skip(1).Select(b => new BingoBoard(b)).ToList();

			long left = boards.Count;
			foreach (var number in numbers)
			{
				for (int i = 0; i < boards.Count; i++)
				{
					var b = boards[i];
					if (b.MarkNumber(number) is long[] row)
					{
						left--;
						if (left == 0)
						{
							return b.UnmarkedNumbers() * number;
						}
						else
						{
							boards.RemoveAt(i);
							i--;
						}
					}
				}
			}

			return null;
		}
	}

	class BingoBoard
	{
		public long[][] Numbers { get; }
		public bool[,] Marks { get; }

		public BingoBoard(string board)
		{
			Numbers = board.Lines()
				.Select(line => line.Split(' ', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries).Select(long.Parse).ToArray())
				.ToArray();
			Marks = new bool[5, 5];
		}

		public long[]? MarkNumber(long number)
		{
			for (int i = 0; i < 5; i++)
			{
				for (int j = 0; j < 5; j++)
				{
					if (Numbers[i][j] == number)
					{
						Marks[i, j] = true;

						bool all = true;
						List<long> row = new List<long>();
						for (int k = 0; all && k < 5; k++)
						{
							all &= Marks[i, k];
							row.Add(Numbers[i][k]);
						}
						if (all)
						{
							return row.ToArray();
						}
						all = true;
						row.Clear();
						for (int k = 0; all && k < 5; k++)
						{
							all &= Marks[k, j];
							row.Add(Numbers[k][j]);
						}
						if (all)
						{
							return row.ToArray();
						}

						return null;
					}
				}
			}

			return null;
		}

		public long UnmarkedNumbers()
		{
			long sum = 0;
			for (int i = 0; i < 5; i++)
			{
				for (int j = 0; j < 5; j++)
				{
					if (!Marks[i, j])
					{
						sum += Numbers[i][j];
					}
				}
			}
			return sum;
		}
	}
}
