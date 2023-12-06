using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2023;

[Day(6)]
public class Day6
{
	[Part(1)]
	public object Part1(string input)
	{
		var lines = input.Lines();
		var races = lines[0][5..].Words().Select(long.Parse).Zip(lines[1][9..].Words().Select(long.Parse));
		return races
			.Select(StartButtonTimes)
			.Select(Enumerable.LongCount)
			.Product();
	}

	private static IEnumerable<long> StartButtonTimes((long, long) race)
	{
		var (time, distance) = race;
		for (long t = 1; t < time; t++)
		{
			if ((time - t) * t >= distance)
			{
				yield return t;
			}
		}
	}

	[Part(2)]
	public object Part2(string input)
	{
		var lines = input.Lines();
		var race = (long.Parse(string.Concat(lines[0][5..].Words())), long.Parse(string.Concat(lines[1][9..].Words().Select(long.Parse))));
		return StartButtonTimesCount(race);
	}

	private static long StartButtonTimesCount((long, long) race)
	{
		var (time, distance) = race;
		var min = 0L;
		var max = time;
		for (long t = 1; t < time; t++)
		{
			if ((time - t) * t >= distance)
			{
				min = t;
				break;
			}
		}
		for (long t = time - 1; t > 0; t--)
		{
			if ((time - t) * t >= distance)
			{
				max = t;
				break;
			}
		}
		return max + 1 - min;
	}
}
