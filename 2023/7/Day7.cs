using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;
using RegExtract;

namespace AoC.Year2023;

[Day(7)]
public class Day7
{
	[Part(1)]
	public object Part1(string input) =>
		Solve(input, "AKQJT98765432");

	[Part(2)]
	public object Part2(string input) =>
		Solve(input, "AKQT98765432J", part2: true);

	private long Solve(string input, string strength, bool part2 = false) =>
		input.Lines()
			.Extract<(string hand, long bid)>(@"(.*) (\d+)")
			.OrderBy(p => p.hand, new HandComparer(strength, part2))
			.Select((p, i) => p.bid * (i + 1L))
			.Sum(Id);

	private class HandComparer : IComparer<string>
	{
		private readonly string Strength;
		private readonly bool Part2;

		public HandComparer(string strength, bool part2 = false) =>
			(Strength, Part2) = (strength, part2);

		public int Compare(string a, string b)
		{
			var aKind = GetKind(a);
			var bKind = GetKind(b);
			if (aKind != bKind)
			{
				return aKind.CompareTo(bKind);
			}
			foreach (var (aC, bC) in a.Zip(b))
			{
				if (aC != bC)
				{
					return Strength.IndexOf(bC).CompareTo(Strength.IndexOf(aC));
				}
			}
			return 0;
		}

		private Kind GetKind(string hand)
		{
			var js = hand.Where(c => c == 'J' && Part2).Count();
			var distinct = hand.Where(c => !(c == 'J' && Part2))
				.GroupBy(Id)
				.Select(g => g.ToList())
				.OrderByDescending(g => g.Count)
				.ToList();
			if (distinct.Count is 1 or 0)
			{
				return Kind.FiveKind;
			}
			if (distinct.Count == 2 && distinct[0].Count + js >= 4)
			{
				return Kind.FourKind;
			}
			if (distinct.Count == 2 && distinct[0].Count + js >= 3)
			{
				return Kind.House;
			}
			if (distinct.Count is 3 or 2 && distinct[0].Count + js >= 3)
			{
				return Kind.ThreeKind;
			}
			if (distinct.Count == 3)
			{
				return Kind.TwoPair;
			}
			if (distinct.Count == 4)
			{
				return Kind.OnePair;
			}
			if (distinct.Count == 5)
			{
				return Kind.High;
			}
			(hand, distinct, js).Dump();
			throw new Exception("You shouldn't be here");
		}

		private enum Kind
		{
			High,
			OnePair,
			TwoPair,
			ThreeKind,
			House,
			FourKind,
			FiveKind,
		}
	}
}
