using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;
using static AoC.Library.Functional;

namespace AoC.Year2025;

[Day(11)]
public class Day11
{
	private Dictionary<string, string[]> map;

	[Part(1)]
	public object Part1(string input)
	{
		map = Parse(input);
		return memoizedCountPaths("you", "out");
	}

	[Part(2)]
	public object Part2(string input)
	{
		map = Parse(input);
		return memoizedCountPaths("svr", "dac") * memoizedCountPaths("dac", "fft") * memoizedCountPaths("fft", "out")
			+ memoizedCountPaths("svr", "fft") * memoizedCountPaths("fft", "dac") * memoizedCountPaths("dac", "out");
	}

	private Dictionary<string, string[]> Parse(string input) =>
		input.Lines().Extract<(string, string)>(@"^(.*): (.*)$").ToDictionary(p => p.Item1, p => p.Item2.Words());

	private Func<string, string, long> memoizedCountPaths;

	public Day11()
	{
		memoizedCountPaths = Curry(Memoize(Uncurry<string, string, long>(CountPaths)));
	}

	private long CountPaths(string from, string to)
	{
		if (from == to)
		{
			return 1L;
		}
		return map.TryGetValue(from, out var nexts) ? nexts.Sum(n => memoizedCountPaths(n, to)) : 0L;
	}
}
