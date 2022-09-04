using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;

namespace AoC.Year2015
{
	[Day(11)]
	public class Day11
	{
		private static readonly IReadOnlySet<int> ForbiddenCharacters = new[] { 'i', 'o', 'l' }.Select(c => (int)(c - 'a')).ToHashSet();

		[Part(1)]
		public string Part1(string input)
		{
			var password = ToNumeric(input);
			do
			{
				Increase(password);
			} while (!IsPasswordValid(password));
			return ToAlpha(password);
		}

		[Part(2)]
		public string Part2(string input) =>
			Part1(Part1(input));

		private int[] ToNumeric(string password) =>
			password.Select(c => (int)(c - 'a')).ToArray();

		private string ToAlpha(int[] password) =>
			String.Concat(password.Select(l => (char)(l + 'a')));

		private void Increase(int[] password)
		{
			const int LETTERS = 'z' - 'a' + 1;

			for (int i = password.Length - 1; i >= 0; i--)
			{
				password[i] = MathM.Mod(password[i] + 1, LETTERS);
				if (password[i] != 0)
				{
					return;
				}
			}
		}

		private bool IsPasswordValid(int[] password) =>
			ContainsIncreasing(password) &&
				DoesNotContainConfusingCharacters(password) &&
				ContainsTwoPairs(password);

		private bool ContainsIncreasing(int[] password) =>
			password.Window3()
				.Any(t => t.Item1 + 1 == t.Item2 &&
					t.Item2 + 1 == t.Item3);

		private bool DoesNotContainConfusingCharacters(int[] password) =>
			password.All(Not<int>(ForbiddenCharacters.Contains));

		private bool ContainsTwoPairs(int[] password) =>
			IndexOfPair(password, 0) is int i &&
				IndexOfPair(password, i + 2) is int;

		private int? IndexOfPair(int[] password, int start) =>
			password
				.Skip(start)
				.Window()
				.Select((p, i) => (p, i))
				.Where(t => t.p.Item1 == t.p.Item2)
				.Select(t => (int?)t.i)
				.FirstOrDefault();
	}
}
