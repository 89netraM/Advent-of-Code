#pragma warning disable CS8509

using System;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;
using RegExtract;
using System.Collections.Generic;

namespace AoC.Year2016;

[Day(4)]
public class Day4
{
	[Part(1)]
	public object Part1(string input) =>
		WhereValid(input)
			.Sum(p => p.Item2);

	private IEnumerable<(string, long, string)> WhereValid(string input) =>
		input.Lines()
			.Extract<(string, long, string)>(@"((?:\w+-)+)(\d+)\[(\w+)\]")
			.Where(IsValid);

	private bool IsValid((string d, long, string c) input) =>
		input.d
			.Where(c => c != '-')
			.GroupBy(Id)
			.OrderByDescending(g => g.Count())
			.ThenBy(g => g.Key)
			.Select(g => g.Key)
			.Take(5)
			.SequenceEqual(input.c);

	[Part(2)]
	public object Part2(string input) =>
		WhereValid(input)
			.Single(t => Rotate(t.Item1, t.Item2).StartsWith("north"))
			.Item2;

	private string Rotate(string d, long steps) =>
		String.Concat(d.Select(c => Rotate(c, steps)));

	private char Rotate(char c, long steps) =>
		c switch
		{
			'-' => ' ',
			>= 'a' and <= 'z' => (char)(((c - 'a') + steps) % ('z' - 'a' + 1) + 'a'),
		};
}
