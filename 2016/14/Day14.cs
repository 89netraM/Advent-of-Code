using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using AoC.Library;

namespace AoC.Year2016;

[Day(14)]
public class Day14
{
	private readonly static Regex three = new Regex(@"(.)\1\1");

	[Part(1)]
	public object Part1(string input) =>
		Search(number => Hash.MD5($"{input}{number}"));

	[Part(2)]
	public object Part2(string input) =>
		Search(number => Hash.MD5($"{input}{number}", 2017));

	private long Search(Func<long, string> hashing)
	{
		var found = 0;
		var threes = new List<(string five, long limit)>();
		for (long number = 0; true; number++)
		{
			var hash = hashing(number);
			for (int i = 0; i < threes.Count; i++)
			{
				if (number < threes[i].limit)
				{
					if (hash.Contains(threes[i].five))
					{
						found++;

						if (found == 64)
						{
							return threes[i].limit - 1000;
						}

						threes.RemoveAt(i);
						i--;
					}
				}
				else
				{
					threes.RemoveAt(i);
					i--;
				}
			}

			var match = three.Match(hash);
			if (match.Success)
			{
				var five = new String(match.Groups[1].Value[0], 5);
				threes.Add((five, number + 1000));
			}
		}
	}
}
