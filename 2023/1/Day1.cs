using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2023;

[Day(1)]
public class Day1
{
	[Part(1)]
	public object Part1(string input)
	{
		return input.Lines().Select(l => long.Parse(l.Where(char.IsDigit).First() + "" + l.Where(char.IsDigit).Last())).Sum();
	}

	[Part(2)]
	public object Part2(string input)
	{
		return input.Lines()
			.Select(l => long.Parse(Numb(l).First() + Numb(l).Last()))
			.Sum();
	}

	private static IEnumerable<string> Numb(string l)
	{
		for (int i = 0; i < l.Length; i++)
		{
			if (l[i..] is ['z', 'e', 'r', 'o', ..]) yield return "0";
			if (l[i..] is ['o', 'n', 'e', ..]) yield return "1";
			if (l[i..] is ['t', 'w', 'o', ..]) yield return "2";
			if (l[i..] is ['t', 'h', 'r', 'e', 'e', ..]) yield return "3";
			if (l[i..] is ['f', 'o', 'u', 'r', ..]) yield return "4";
			if (l[i..] is ['f', 'i', 'v', 'e', ..]) yield return "5";
			if (l[i..] is ['s', 'i', 'x', ..]) yield return "6";
			if (l[i..] is ['s', 'e', 'v', 'e', 'n', ..]) yield return "7";
			if (l[i..] is ['e', 'i', 'g', 'h', 't', ..]) yield return "8";
			if (l[i..] is ['n', 'i', 'n', 'e', ..]) yield return "9";
			if (char.IsDigit(l[i])) yield return l[i].ToString();
		}
	}
}
