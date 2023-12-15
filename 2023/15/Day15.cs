using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2023;

[Day(15)]
public class Day15
{
	[Part(1)]
	public object Part1(string input)
	{
		return input.Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
			.Select(Hash)
			.Sum();
	}

	[Part(2)]
	public object Part2(string input)
	{
		var boxes = Enumerable.Range(0, 256).Select(_ => new List<(string, long)>()).ToArray();
		InsertLenses(boxes, input.Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries));
		return FocusingPower(boxes);
	}

	private static long Hash(string str) =>
		str.Aggregate(0L, (v, c) => MathM.Mod((v + c) * 17, 256));

	private static void InsertLenses(List<(string label, long length)>[] boxes, IEnumerable<string> things)
	{
		foreach (var thing in things)
		{
			if (thing.Split('=', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries) is [var label, var value])
			{
				var hash = Hash(label);
				var i = boxes[hash].FindIndex(p => p.label == label);
				if (i >= 0)
				{
					boxes[hash][i] = (label, long.Parse(value));
				}
				else
				{
					boxes[hash].Add((label, long.Parse(value)));
				}
			}
			else if (thing.Split('-', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries) is [var label2])
			{
				var hash = Hash(label2);
				var i = boxes[hash].FindIndex(p => p.label == label2);
				if (i >= 0)
				{
					boxes[hash].RemoveAt(i);
				}
			}
		}
	}

	private static long FocusingPower(List<(string label, long length)>[] boxes) =>
		boxes.Enumerate()
			.Aggregate(0L, (p, ib) => p + ib.Value
				.Enumerate()
				.Sum(sll => (ib.Key + 1) * (sll.Key + 1) * sll.Value.length));
}
