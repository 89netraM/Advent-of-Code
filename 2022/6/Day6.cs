using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2022;

[Day(6)]
public class Day6
{
	[Part(1)]
	public object Part1(string input) =>
		input.Enumerate()
			.Window4()
			.First(p => new HashSet<char> { p.Item1.Value, p.Item2.Value, p.Item3.Value, p.Item4.Value }.Count == 4)
			.Item4.Key + 1;

	[Part(2)]
	public object Part2(string input)
	{
		var inputs = new List<char[]>();
		for (int i = 0; i < 14; i++)
		{
			inputs.Add(input.Skip(i).ToArray());
		}

		var set = new HashSet<char>();
		for (int i = 0; i < input.Length - 14; i++)
		{
			set.Clear();
			for (int j = 0; j < 14; j++)
			{
				set.Add(inputs[j][i]);
			}
			if (set.Count == 14)
			{
				return i + 14;
			}
		}

		return null;
	}
}
