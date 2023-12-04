using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2023;

[Day(4)]
public class Day4
{
	[Part(1)]
	public object Part1(string input)
	{
		return input.Lines()
			.Extract<(long id, string win, string my)>(@"Card *?(\d+): (.*) \| (.*)")
			.Select(p => (p.id, win: p.win.Split(" ", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries).Select(long.Parse).ToHashSet(), my: p.my.Split(" ", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries).Select(long.Parse).ToHashSet()))
			.Sum(p =>
			{
				p.win.IntersectWith(p.my);
				return p.win.Count == 0 ? 0 : Math.Pow(2, p.win.Count - 1);
			});
	}

	[Part(2)]
	public object Part2(string input)
	{
		var cards = input.Lines()
			.Extract<(long id, string win, string my)>(@"Card *?(\d+): (.*) \| (.*)")
			.Select(p => (p.id, win: p.win.Split(" ", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries).Select(long.Parse).ToHashSet(), my: p.my.Split(" ", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries).Select(long.Parse).ToHashSet()))
			.ToDictionary(p => p.id, p =>
			{
				p.win.IntersectWith(p.my);
				return p.win.Count;
			});
		var counter = new Dictionary<long, long>();
		return cards.Sum(p =>
		{
			var count = counter.GetValueOrDefault(p.Key, 0) + 1;
			for (long l = 0; l < p.Value; l++)
			{
				counter.Increase(p.Key + l + 1, count);
			}
			return count;
		});
	}
}
