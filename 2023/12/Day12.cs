using System;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;
using System.Collections.Immutable;
using System.Collections.Generic;

namespace AoC.Year2023;

[Day(12)]
public class Day12
{
	[Part(1)]
	public object Part1(string input)
	{
		return input.Lines()
			.Select(Parse)
			.Sum(CountArrangements);
	}

	[Part(2)]
	public object Part2(string input)
	{
		return input.Lines()
			.Select(Parse)
			.Select(Unfold)
			.AsParallel()
			.Sum(CountArrangements);
	}

	private static (ImmutableArray<Equipment>, ImmutableArray<int>) Parse(string line)
	{
		var parts = line.Split(' ');
		return (
			parts[0]
				.Select(c => c switch
				{
					'#' => Equipment.Broken,
					'.' => Equipment.Operational,
					_ => Equipment.Unknown,
				})
				.ToImmutableArray(),
			parts[1].Split(',')
				.Select(int.Parse)
				.ToImmutableArray()
		);
	}

	private static (ImmutableArray<Equipment>, ImmutableArray<int>) Unfold((ImmutableArray<Equipment>, ImmutableArray<int>) input)
	{
		var (equipment, groups) = input;
		return (
			Enumerable.Range(0, 5)
				.SelectMany(Const<int, IEnumerable<Equipment>>(equipment.Append(Equipment.Unknown)))
				.SkipLast(1)
				.ToImmutableArray(),
			Enumerable.Range(0, 5)
				.SelectMany(Const<int, IEnumerable<int>>(groups))
				.ToImmutableArray()
		);
	}

	private long CountArrangements((ImmutableArray<Equipment>, ImmutableArray<int>) input)
	{
		Func<ReadOnlyMemory<Equipment>, ReadOnlyMemory<int>, long> memoizedCountArrangementsRecursive = null!;
		memoizedCountArrangementsRecursive = Curry(Memoize(Uncurry<ReadOnlyMemory<Equipment>, ReadOnlyMemory<int>, long>(CountArrangementsRecursive)));
		var (equipment, groups) = input;
		return memoizedCountArrangementsRecursive(equipment.AsMemory(), groups.AsMemory());

		long CountArrangementsRecursive(ReadOnlyMemory<Equipment> equipment, ReadOnlyMemory<int> groups)
		{
			if (groups.Length == 0)
			{
				if (equipment.All(e => e is not Equipment.Broken))
				{
					return 1;
				}
				else
				{
					return 0;
				}
			}
			if (equipment.Length == 0)
			{
				return 0;
			}
			// Both contain something
			if (equipment.Span[0] is Equipment.Operational)
			{
				var operationalCount = equipment.CountPrefix(e => e == Equipment.Operational);
				return memoizedCountArrangementsRecursive(equipment[operationalCount..], groups);
			}
			// We start on a # or a ?
			var group = groups.Span[0];
			if (equipment.Length < group)
			{
				return 0;
			}
			if (equipment.Span[0] is Equipment.Broken)
			{
				if (equipment[..group].All(e => e is not Equipment.Operational))
				{
					if (equipment.Length == group)
					{
						return groups.Length == 1
							? 1
							: 0;
					}
					else if (equipment.Span[group] is not Equipment.Broken)
					{
						return memoizedCountArrangementsRecursive(equipment[(group + 1)..], groups[1..]);
					}
					else
					{
						return 0;
					}
				}
				else
				{
					return 0;
				}
			}
			// The first is unknown
			if (equipment[..group].All(e => e is not Equipment.Operational))
			{
				if (equipment.Length == group)
				{
					return groups.Length == 1
						? 1
						: 0;
				}
				else if (equipment.Span[group] is not Equipment.Broken)
				{
					return memoizedCountArrangementsRecursive(equipment[(group + 1)..], groups[1..]) + memoizedCountArrangementsRecursive(equipment[1..], groups);
				}
			}
			return memoizedCountArrangementsRecursive(equipment[1..], groups);
		}
	}

	private enum Equipment
	{
		Unknown,
		Operational,
		Broken,
	}
}
