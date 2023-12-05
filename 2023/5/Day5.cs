using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

#nullable enable

namespace AoC.Year2023;

[Day(5)]
public class Day5
{
	[Part(1)]
	public object Part1(string input)
	{
		var (seeds, maps) = Parse(input);
		foreach (var map in maps)
		{
			for (int i = 0; i < seeds.Count; i++)
			{
				foreach (var range in map)
				{
					if (range.IsInRange(seeds[i]))
					{
						seeds[i] = range.Move(seeds[i]);
						break;
					}
				}
			}
		}
		return seeds.Min();
	}

	[Part(2)]
	public object Part2(string input)
	{
		var (seedRanges, maps) = Parse(input);
		var seeds = seedRanges.Chunk(2)
			.Select(Seed.FromChunk);
		foreach (var map in maps)
		{
			seeds = seeds.SelectMany(seed => Plant(map, seed));
		}
		return seeds.Min(s => s.Min);
	}

	private static IEnumerable<Seed> Plant(List<Range> map, Seed seed)
	{
		foreach (var range in map)
		{
			if (range.SourceMax < seed.Min)
			{
				continue;
			}
			if (seed.Max < range.Source)
			{
				break;
			}
			if (seed.Min < range.Source)
			{
				yield return seed with { Max = range.Source - 1 };
				seed = seed with { Min = range.Source };
			}
			if (seed.Max < range.SourceMax)
			{
				yield return new(range.Move(seed.Min), range.Move(seed.Max));
				break;
			}
			else
			{
				yield return new(range.Move(seed.Min), range.Move(range.SourceMax));
				seed = seed with { Min = range.SourceMax + 1 };
			}
		}
		if (map[^1].SourceMax < seed.Min) {
			yield return seed;
		}
	}

	private static (List<long>, List<List<Range>>) Parse(string input)
	{
		var sections = input.Split("\n\n", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries);
		var seeds = sections[0][7..].Split(" ", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries)
			.Select(long.Parse)
			.ToList();
		var maps = sections.Skip(1)
			.Select(m => m.Lines()
				.Skip(1)
				.Extract<Range>(@"(\d+) (\d+) (\d+)")
				.OrderBy(r => r.Source)
				.ToList())
			.ToList();
		return (seeds, maps);
	}

	record Range(long Destination, long Source, long Length)
	{
		public long SourceMax => Source + Length - 1;

		public bool IsInRange(long seed) =>
			Source <= seed && seed < Source + Length;

		public long Move(long seed) =>
			seed + Destination - Source;
	}

	record Seed(long Min, long Max)
	{
		public static Seed FromChunk(long[] chunk) =>
			new(chunk[0], chunk[0] + chunk[1] - 1);
	}
}
