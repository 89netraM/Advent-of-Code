using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;
using System.Collections.Immutable;

namespace AoC.Year2015
{
	[Day(21)]
	public class Day21
	{
		private static readonly IReadOnlyDictionary<string, (long c, long d, long a)> Weapons = new Dictionary<string, (long, long, long)>(
			@"Dagger        8     4       0
			Shortsword   10     5       0
			Warhammer    25     6       0
			Longsword    40     7       0
			Greataxe     74     8       0"
				.Lines()
				.Extract<KeyValuePair<string, (long, long, long)>>(@"\s*(.*?)\s*((\d+)\s*(\d+)\s*(\d+))$")
		);
		private static readonly IReadOnlyDictionary<string, (long c, long d, long a)> Armor = new Dictionary<string, (long, long, long)>(
			@"Leather      13     0       1
			Chainmail    31     0       2
			Splintmail   53     0       3
			Bandedmail   75     0       4
			Platemail   102     0       5"
				.Lines()
				.Extract<KeyValuePair<string, (long, long, long)>>(@"\s*(.*?)\s*((\d+)\s*(\d+)\s*(\d+))$")
		);
		private static readonly IReadOnlyDictionary<string, (long c, long d, long a)> Rings = new Dictionary<string, (long, long, long)>(
			@"Damage +1    25     1       0
			Damage +2    50     2       0
			Damage +3   100     3       0
			Defense +1   20     0       1
			Defense +2   40     0       2
			Defense +3   80     0       3"
				.Lines()
				.Extract<KeyValuePair<string, (long, long, long)>>(@"\s*(.*?)\s*((\d+)\s*(\d+)\s*(\d+))$")
		);

		[Part(1)]
		public object Part1(string input)
		{
			var boss = ParseInput(input);
			return PossibleWeapons()
				.Where(s => s.d - boss.a >= boss.d - s.a)
				.Min(s => s.c);
		}

		[Part(2)]
		public object Part2(string input)
		{
			var boss = ParseInput(input);
			return PossibleWeapons()
				.Where(s => s.d - boss.a < boss.d - s.a)
				.Max(s => s.c);
		}

		private (long hp, long d, long a) ParseInput(string input)
		{
			var lines = input.Lines().Select(l => Int64.Parse(l.Split(": ")[1])).ToArray();
			return (lines[0], lines[1], lines[2]);
		}

		private IEnumerable<(long c, long d, long a, IImmutableSet<string> l)> PossibleWeapons()
		{
			foreach (var (k, (c, d, a)) in Weapons)
			{
				foreach (var (ic, id, ia, il) in PossibleArmors())
				{
					yield return (c + ic, d + id, a + ia, il.Add(k));
				}
			}
		}

		private IEnumerable<(long, long, long, IImmutableSet<string>)> PossibleArmors()
		{
			foreach (var (k, (c, d, a)) in Armor.Append(new("", (0, 0, 0))))
			{
				foreach (var (ic, id, ia, il) in PossibleRings())
				{
					yield return (c + ic, d + id, a + ia, il.Add(k));
				}
			}
		}

		private IEnumerable<(long, long, long, IImmutableSet<string>)> PossibleRings()
		{
			yield return (0, 0, 0, ImmutableHashSet.Create(""));
			foreach (var (k, (c, d, a)) in Rings)
			{
				yield return (c, d, a, ImmutableHashSet.Create(k));
				foreach (var (k2, (c2, d2, a2)) in Rings)
				{
					yield return (c + c2, d + d2, a + a2, ImmutableHashSet.Create(k, k2));
				}
			}
		}
	}
}
