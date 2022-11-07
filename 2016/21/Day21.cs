#pragma warning disable CS8509

using System;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2016;

[Day(21)]
public class Day21
{
	[Part(1)]
	public object Part1(string input)
	{
		Span<char> password = "abcdefgh".ToArray();

		foreach (var instruction in input.Lines())
		{
			if (instruction.StartsWith("swap position"))
			{
				var (from, to) = instruction.Extract<(int, int)>(@"position (\d+) with position (\d+)");
				(password[from], password[to]) = (password[to], password[from]);
			}
			else if (instruction.StartsWith("swap letter"))
			{
				var (a, b) = instruction.Extract<(char, char)>(@"letter (.) with letter (.)");
				var from = password.IndexOf(a);
				var to = password.IndexOf(b);
				(password[from], password[to]) = (password[to], password[from]);
			}
			else if (instruction.StartsWith("rotate based"))
			{
				var letter = instruction.Extract<char>(@"letter (.)");
				var index = password.IndexOf(letter);
				var rotations = 1 + index;
				if (index >= 4)
				{
					rotations++;
				}
				RotateRight(ref password, rotations);
			}
			else if (instruction.StartsWith("rotate"))
			{
				var rotations = instruction.Extract<int>(@"(\d+) step");
				if (instruction.StartsWith("rotate left"))
				{
					RotateLeft(ref password, rotations);
				}
				else
				{
					RotateRight(ref password, rotations);
				}
			}
			else if (instruction.StartsWith("reverse"))
			{
				var (from, to) = instruction.Extract<(int, int)>(@"positions (\d+) through (\d+)");
				Reverse(ref password, from, to);
			}
			else if (instruction.StartsWith("move"))
			{
				var (from, to) = instruction.Extract<(int, int)>(@"position (\d+) to position (\d+)");
				Move(ref password, from, to);
			}
		}

		return new String(password);
	}

	[Part(2)]
	public object Part2(string input)
	{
		Span<char> password = "fbgdceah".ToArray();

		foreach (var instruction in input.Lines().Reverse())
		{
			if (instruction.StartsWith("swap position"))
			{
				var (from, to) = instruction.Extract<(int, int)>(@"position (\d+) with position (\d+)");
				(password[from], password[to]) = (password[to], password[from]);
			}
			else if (instruction.StartsWith("swap letter"))
			{
				var (a, b) = instruction.Extract<(char, char)>(@"letter (.) with letter (.)");
				var from = password.IndexOf(a);
				var to = password.IndexOf(b);
				(password[from], password[to]) = (password[to], password[from]);
			}
			else if (instruction.StartsWith("rotate based"))
			{
				var letter = instruction.Extract<char>(@"letter (.)");
				var currentIndex = password.IndexOf(letter);

				var rotations = currentIndex switch
				{
					0 => 9,
					1 => 1,
					2 => 6,
					3 => 2,
					4 => 7,
					5 => 3,
					6 => 8,
					7 => 4,
				};

				RotateLeft(ref password, rotations);
			}
			else if (instruction.StartsWith("rotate"))
			{
				var rotations = instruction.Extract<int>(@"(\d+) step");
				if (instruction.StartsWith("rotate left"))
				{
					RotateRight(ref password, rotations);
				}
				else
				{
					RotateLeft(ref password, rotations);
				}
			}
			else if (instruction.StartsWith("reverse"))
			{
				var (from, to) = instruction.Extract<(int, int)>(@"positions (\d+) through (\d+)");
				Reverse(ref password, from, to);
			}
			else if (instruction.StartsWith("move"))
			{
				var (from, to) = instruction.Extract<(int, int)>(@"position (\d+) to position (\d+)");
				Move(ref password, to, from);
			}
		}

		return new String(password);
	}

	private void RotateLeft(ref Span<char> password, int rotations)
	{
		for (; rotations > 0; rotations--)
		{
			for (int i = 1; i < password.Length; i++)
			{
				(password[i - 1], password[i]) = (password[i], password[i - 1]);
			}
		}
	}

	private void RotateRight(ref Span<char> password, int rotations)
	{
		for (; rotations > 0; rotations--)
		{
			for (int i = password.Length - 2; i >= 0; i--)
			{
				(password[i + 1], password[i]) = (password[i], password[i + 1]);
			}
		}
	}

	private void Reverse(ref Span<char> password, int from, int to)
	{
		if (from > to)
		{
			(from, to) = (to, from);
		}

		var length = to - from + 1;
		for (int i = 0; i < length / 2; i++)
		{
			(password[from + i], password[to - i]) = (password[to - i], password[from + i]);
		}
	}

	private void Move(ref Span<char> password, int from, int to)
	{
		if (from < to)
		{
			for (int i = from + 1; i <= to; i++)
			{
				(password[i - 1], password[i]) = (password[i], password[i - 1]);
			}
		}
		else if (from > to)
		{
			for (int i = from - 1; i >= to; i--)
			{
				(password[i + 1], password[i]) = (password[i], password[i + 1]);
			}
		}
	}
}
