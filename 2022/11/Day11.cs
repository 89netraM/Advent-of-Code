using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2022;

[Day(11)]
public class Day11
{
	[Part(1)]
	public object Part1(string input)
	{
		var monkeys = input.Split("\n\n")
			.Select(ParseMonkey)
			.ToArray();

		var business = new long[monkeys.Length];
		for (int i = 0; i < 20; i++)
		{
			for (int m = 0; m < monkeys.Length; m++)
			{
				while (monkeys[m].Items.Count > 0)
				{
					var item = monkeys[m].Items[^1];
					monkeys[m].Items.RemoveAt(monkeys[m].Items.Count - 1);

					item = monkeys[m].Operation(item) / 3;
					if ((item % monkeys[m].Test) == 0)
					{
						monkeys[monkeys[m].TrueTarget].Items.Add(item);
					}
					else
					{
						monkeys[monkeys[m].FalseTarget].Items.Add(item);
					}

					business[m]++;
				}
			}
		}

		return business.OrderDescending().Take(2).Product();
	}

	[Part(2)]
	public object Part2(string input)
	{
		var monkeys = input.Split("\n\n")
			.Select(ParseMonkey)
			.ToArray();

		long lcm = monkeys.Select(m => m.Test).Lcm();

		var business = new long[monkeys.Length];
		for (int i = 0; i < 10000; i++)
		{
			for (int m = 0; m < monkeys.Length; m++)
			{
				while (monkeys[m].Items.Count > 0)
				{
					var item = monkeys[m].Items[^1];
					monkeys[m].Items.RemoveAt(monkeys[m].Items.Count - 1);

					item = monkeys[m].Operation(item) % lcm;
					if ((item % monkeys[m].Test) == 0)
					{
						monkeys[monkeys[m].TrueTarget].Items.Add(item);
					}
					else
					{
						monkeys[monkeys[m].FalseTarget].Items.Add(item);
					}

					business[m]++;
				}
			}
		}

		return business.OrderDescending().Take(2).Product();
	}

	private Monkey ParseMonkey(string input)
	{
		var lines = input.Lines();
		return new Monkey(
			lines[1].Extract<List<long>>(@"(?:(\d+)(?:, )?)+"),
			ParseOperation(lines[2]),
			lines[3].Extract<long>(@"(\d+)"),
			lines[4].Extract<int>(@"(\d+)"),
			lines[5].Extract<int>(@"(\d+)"));
	}

	private Func<long, long> ParseOperation(string input)
	{
		if (long.TryParse(input[25..], out long c))
		{
			if (input[23] == '*')
			{
				return old => old * c;
			}
			else
			{
				return old => old + c;
			}
		}
		else
		{
			if (input[23] == '*')
			{
				return old => old * old;
			}
			else
			{
				return old => old + old;
			}
		}
	}

	record Monkey(List<long> Items, Func<long, long> Operation, long Test, int TrueTarget, int FalseTarget);
}
