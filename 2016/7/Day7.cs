using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using AoC.Library;

namespace AoC.Year2016;

[Day(7)]
public class Day7
{
	Regex abbaRegex = new Regex(@"(.)(.)\2\1");
	Regex abaRegex = new Regex(@"(.)(.)\1");

	[Part(1)]
	public object Part1(string input) =>
		input.Lines()
			.Where(l => IsABBA(l) && Parsing.ParseParentheses(l, '[', ']').All(p => !IsABBA(p)))
			.Count();

	bool IsABBA(string str) =>
		abbaRegex.Match(str)
			.Let(m => m.Success && m.Groups[1].Value != m.Groups[2].Value);

	[Part(2)]
	public object Part2(string input) =>
		input.Lines()
			.Where(l => RemoveInParentheses(l).SelectMany(FindABA).Any(ssl => Parsing.ParseParentheses(l, '[', ']').Any(p => p.Contains(ssl))))
			.Count();

	IEnumerable<string> FindABA(string str)
	{
		for (var match = abaRegex.Match(str); match.Success; match = abaRegex.Match(str, match.Index + 1))
		{
			if (match.Groups[1].Value != match.Groups[2].Value)
			{
				yield return match.Groups[2].Value + match.Groups[1].Value + match.Groups[2].Value;
			}
		}
	}

	IEnumerable<string> RemoveInParentheses(string str)
	{
		var level = 0;
		var entryIndex = 0;
		for (int i = 0; i < str.Length; i++)
		{
			switch (str[i])
			{
				case '[':
					if (level == 0)
					{
						yield return str[entryIndex..i];
					}
					level++;
					break;
				case ']':
					level--;
					if (level == 0)
					{
						entryIndex = i + 1;
					}
					break;
			}
		}
		if (level == 0)
		{
			yield return str[entryIndex..];
		}
	}
}
