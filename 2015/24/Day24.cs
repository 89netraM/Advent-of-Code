using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using System.Collections.Immutable;

namespace AoC.Year2015
{
	[Day(24)]
	public class Day24
	{
		[Part(1)]
		public object Part1(string input)
		{
			var packets = input.Lines().Select(Int64.Parse).ToArray();
			return Solve(packets, 3);
		}

		[Part(2)]
		public object Part2(string input)
		{
			var packets = input.Lines().Select(Int64.Parse).ToArray();
			return Solve(packets, 4);
		}

		public long Solve(long[] packets, long spaces)
		{
			var weight = packets.Sum() / spaces;
			return GenerateCombinations(packets, weight)
				.Where(s => s.sum == weight && (weight - s.sum) % (spaces - 1) == 0)
				.OrderBy(s => s.s.Count)
				.ThenBy(s => s.s.Product())
				.First()
				.s
				.Product();
		}

		private IEnumerable<(IImmutableSet<long> s, long sum)> GenerateCombinations(long[] packets, long maxWeight, int i = 0)
		{
			if (i < packets.Length)
			{
				yield return (ImmutableHashSet.Create(packets[i]), packets[i]);
				foreach (var inner in GenerateCombinations(packets, maxWeight, i + 1))
				{
					yield return inner;
					if (inner.sum + packets[i] <= maxWeight)
					{
						yield return (inner.s.Add(packets[i]), inner.sum + packets[i]);
					}
				}
			}
		}
	}
}
