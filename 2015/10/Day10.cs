using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;

namespace AoC.Year2015
{
	[Day(10)]
	public class Day10
	{
		[Part(1)]
		public object Part1(string input) =>
			LengthOfNLookAndSayIterations(input, 40);

		[Part(2)]
		public object Part2(string input) =>
			LengthOfNLookAndSayIterations(input, 50);

		private int LengthOfNLookAndSayIterations(IEnumerable<char> input, int n)
		{
			IEnumerable<char> sequence = input;
			for (int i = 0; i < n; i++)
			{
				sequence = LookAndSay(sequence);
			}
			return sequence.Count();
		}

		private IEnumerable<char> LookAndSay(IEnumerable<char> input) =>
			input.AdjacentGroupBy(Id).SelectMany(g => $"{g.Count()}{g.Key}");
	}
}
