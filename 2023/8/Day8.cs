using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2023;

[Day(8)]
public class Day8
{
	[Part(1)]
	public object Part1(string input)
	{
		var (lr, map) = Parse(input);

		return FindCycle(lr, map, "AAA").toZ;
	}

	[Part(2)]
	public object Part2(string input)
	{
		var (lr, map) = Parse(input);

		return map.Keys
			.Where(n => n[2] == 'A')
			.Select(n => FindCycle(lr, map, n).toZ)
			.Lcm();
	}

	private static (string, Dictionary<string, (string l, string r)>) Parse(string input)
	{
		var sections = input.Split("\n\n", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
		var lr = sections[0];
		var map = sections[1].Lines()
			.Extract<(string, string, string)>(@"(.*?) = \((.*?), (.*?)\)")
			.ToDictionary(p => p.Item1, p => (l: p.Item2, r: p.Item3));
		return (lr, map);
	}

	private static (long toZ, long cycle) FindCycle(string lr, Dictionary<string, (string l, string r)> map, string node)
	{
		var lengthTo = new Dictionary<string, long> { [node] = 0 };
		long count = 0;
		while (true)
		{
			node = Step();
			lengthTo[node] = count;
			if (node[2] == 'Z')
			{
				node = Step();
				return (count - 1, count - lengthTo[node]);
			}
		}

		string Step() =>
			lr[(int)(count++ % lr.Length)] switch
			{
				'L' => map[node].l,
				'R' => map[node].r,
				_ => throw new Exception(),
			};
	}
}
