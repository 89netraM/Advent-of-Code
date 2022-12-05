using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2022;

[Day(5)]
public class Day5
{
	[Part(1)]
	public object Part1(string input)
	{
		var parts = input.Split("\n\n");
		var stacks = parts[0].Lines()
			.Take(8)
			.Transpose()
			.Select(cs => String.Concat(cs).Trim())
			.Where(cs => cs.All(Char.IsAsciiLetter) && cs.Length > 0)
			.Select(cs => new Stack<char>(cs.Reverse()))
			.ToArray();
		var moves = parts[1].Lines()
			.Extract<(int, int, int)>(@"move (\d+) from (\d+) to (\d+)");

		foreach (var (count, from, to) in moves)
		{
			for (int i = 0; i < count; i++)
			{
				var c = stacks[from - 1].Pop();
				stacks[to - 1].Push(c);
			}
		}

		return String.Concat(stacks.Select(s => s.Peek()));
	}

	[Part(2)]
	public object Part2(string input)
	{
		var parts = input.Split("\n\n");
		var stacks = parts[0].Lines()
			.Take(8)
			.Transpose()
			.Select(cs => String.Concat(cs).Trim())
			.Where(cs => cs.All(Char.IsAsciiLetter) && cs.Length > 0)
			.Select(cs => new Stack<char>(cs.Reverse()))
			.ToArray();
		var moves = parts[1].Lines()
			.Extract<(int, int, int)>(@"move (\d+) from (\d+) to (\d+)");

		foreach (var (count, from, to) in moves)
		{
			var temp = new Stack<char>();
			for (int i = 0; i < count; i++)
			{
				temp.Push(stacks[from - 1].Pop());
			}
			for (int i = 0; i < count; i++)
			{
				stacks[to - 1].Push(temp.Pop());
			}
		}

		return String.Concat(stacks.Select(s => s.Peek()));
	}
}
