using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2023;

[Day(9)]
public class Day9
{
	[Part(1)]
	public object Part1(string input)
	{
		return input.Lines()
			.Select(l => l.Words().Select(long.Parse))
			.Sum(ExtrapolateForward);
	}

	private static long ExtrapolateForward(IEnumerable<long> input) =>
		Extrapolate(input, (h, i) => h[i].Add(h[i][^1] + h[i + 1][^1]), h => h[0][^1]);

	[Part(2)]
	public object Part2(string input)
	{
		return input.Lines()
			.Select(l => l.Words().Select(long.Parse))
			.Sum(ExtrapolateBackwards);
	}

	private static long ExtrapolateBackwards(IEnumerable<long> input) =>
		Extrapolate(input, (h, i) => h[i].Insert(0, h[i][0] - h[i + 1][0]), h => h[0][0]);

	private static long Extrapolate(IEnumerable<long> input, Action<List<List<long>>, int> extrapolate, Func<List<List<long>>, long> result)
	{
		var history = new List<List<long>> { input.ToList() };
		while (history[^1].Distinct().Count() > 1)
		{
			history.Add(Next(history[^1]).ToList());
		}

		for (int i = history.Count - 2; i >= 0; i--)
		{
			extrapolate(history, i);
		}

		return result(history);
	}

	private static IEnumerable<long> Next(IEnumerable<long> history) =>
		history.Skip(1).Zip(history, (b, a) => b - a);
}
