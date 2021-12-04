using System;
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
			var boards = input.Split("\n\n").Skip(1).Select(BingoBoard.Parse).ToList();

			foreach (var number in numbers)
			{
				foreach (var b in boards)
				{
					if (b.MarkNumber(number) is long[] row)
					{
						return b.SumOfUnmarkedNumbers() * number;
					}
				}
			}

			return null;
		}

		[Part(2)]
		public object? Part2(string input)
		{
			var numbers = input.Split("\n\n").First().Split(",").Select(long.Parse).ToArray();
			var boards = input.Split("\n\n").Skip(1).Select(BingoBoard.Parse).ToList();

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
							return b.SumOfUnmarkedNumbers() * number;
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

	public class BingoBoard
	{
		public static BingoBoard Parse(string input)
		{
			var numbers = input.Lines()
				.Select(static line => line.Split(' ', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries).Select(long.Parse).ToArray())
				.ToArray();
			BingoBoard board = new BingoBoard(numbers[0].Length, numbers.Length);
			for (int y = 0; y < numbers.Length; y++)
			{
				for (int x = 0; x < numbers[y].Length; x++)
				{
					board.Numbers[x, y] = numbers[y][x];
				}
			}
			return board;
		}

		public long[,] Numbers { get; }
		public bool[,] Marks { get; }

		public int Width => Numbers.GetLength(0);
		public int Height => Numbers.GetLength(1);

		public BingoBoard(int size) : this(new long[size, size]) { }
		public BingoBoard(int width, int height) : this(new long[width, height]) { }
		public BingoBoard(long[,] numbers) : this(numbers, new bool[numbers.GetLength(0), numbers.GetLength(1)]) { }
		public BingoBoard(long[,] numbers, bool[,] marks)
		{
			Numbers = numbers;
			Marks = marks;
		}

		public long[]? MarkNumber(long number)
		{
			for (int x = 0; x < Width; x++)
			{
				for (int y = 0; y < Height; y++)
				{
					if (Numbers[x, y] == number)
					{
						Marks[x, y] = true;

						bool all = true;
						long[] row = new long[Height];
						for (int k = 0; all && k < Height; k++)
						{
							all &= Marks[x, k];
							row[k] = Numbers[x, k];
						}
						if (all)
						{
							return row;
						}

						all = true;
						row = new long[Width];
						for (int k = 0; all && k < Width; k++)
						{
							all &= Marks[k, y];
							row[k] = Numbers[k, y];
						}
						if (all)
						{
							return row;
						}

						return null;
					}
				}
			}

			return null;
		}

		public long SumOfMarkedNumbers()
		{
			long sum = 0;
			for (int x = 0; x < Width; x++)
			{
				for (int y = 0; y < Height; y++)
				{
					if (Marks[x, y])
					{
						sum += Numbers[x, y];
					}
				}
			}
			return sum;
		}

		public long SumOfUnmarkedNumbers()
		{
			long sum = 0;
			for (int x = 0; x < Width; x++)
			{
				for (int y = 0; y < Height; y++)
				{
					if (!Marks[x, y])
					{
						sum += Numbers[x, y];
					}
				}
			}
			return sum;
		}
	}
}
