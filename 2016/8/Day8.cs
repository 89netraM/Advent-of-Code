using System;
using System.Collections.Generic;
using AoC.Library;
using RegExtract;

namespace AoC.Year2016;

[Day(8)]
public class Day8
{
	[Part(1)]
	public object Part1(string input)
	{
		var screen = new bool[50,6];
		FollowInstructions(screen, input.Lines());
		return CountOn(screen);
	}

	[Part(2)]
	public void Part2(string input)
	{
		var screen = new bool[50,6];
		FollowInstructions(screen, input.Lines());
		Print(screen);
	}

	private void FollowInstructions(bool[,] screen, IEnumerable<string> instructions)
	{
		foreach (var ins in instructions)
		{
			if (ins.StartsWith("rect"))
			{
				var (width, height) = ins.Extract<(int, int)>(@"rect (\d+)x(\d+)");
				Rect(screen, width, height);
			}
			else if (ins.StartsWith("rotate column"))
			{
				var (x, steps) = ins.Extract<(int, int)>(@"rotate column x=(\d+) by (\d+)");
				RotateColumn(screen, x, steps);
			}
			else if (ins.StartsWith("rotate row"))
			{
				var (y, steps) = ins.Extract<(int, int)>(@"rotate row y=(\d+) by (\d+)");
				RotateRow(screen, y, steps);
			}
		}
	}

	private void Rect(bool[,] screen, int width, int height)
	{
		for (int x = 0; x < width && x < screen.GetLength(0); x++)
		{
			for (int y = 0; y < height && y < screen.GetLength(1); y++)
			{
				screen[x, y] = true;
			}
		}
	}

	private void RotateColumn(bool[,] screen, int x, int steps)
	{
		var previous = new bool[screen.GetLength(1)];
		for (int y = 0; y < previous.Length; y++)
		{
			previous[y] = screen[x, y];
		}
		for (int y = 0; y < previous.Length; y++)
		{
			screen[x, y] = previous[MathM.Mod(y - steps, previous.Length)];
		}
	}

	private void RotateRow(bool[,] screen, int y, int steps)
	{
		var previous = new bool[screen.GetLength(0)];
		for (int x = 0; x < previous.Length; x++)
		{
			previous[x] = screen[x, y];
		}
		for (int x = 0; x < previous.Length; x++)
		{
			screen[x, y] = previous[MathM.Mod(x - steps, previous.Length)];
		}
	}

	private int CountOn(bool[,] screen)
	{
		int count = 0;
		for (int x = 0; x < screen.GetLength(0); x++)
		{
			for (int y = 0; y < screen.GetLength(1); y++)
			{
				if (screen[x, y])
				{
					count++;
				}
			}
		}
		return count;
	}

	private void Print(bool[,] screen)
	{
		for (int y = 0; y < screen.GetLength(1); y++)
		{
			for (int x = 0; x < screen.GetLength(0); x++)
			{
				Console.Write(screen[x, y] ? '#' : '.');
			}
			Console.WriteLine();
		}
	}
}
