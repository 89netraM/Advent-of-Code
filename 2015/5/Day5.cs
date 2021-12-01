using System.Linq;
using System.Text.RegularExpressions;
using AoC.Library;

namespace AoC.Year2015
{
	[Day(5)]
	public class Day5
	{
		[Part(1)]
		public object Part1(string input)
		{
			Regex vowels = new Regex(@"([aeiou].*?){3}");
			Regex pair = new Regex(@"(\w)\1");
			Regex nono = new Regex(@"ab|cd|pq|xy");
			return input.Lines()
				.Where(l => vowels.IsMatch(l) && pair.IsMatch(l) && !nono.IsMatch(l))
				.Count();
		}

		[Part(2)]
		public object Part2(string input)
		{
			Regex pair = new Regex(@"(\w{2}).*?\1");
			Regex repeat = new Regex(@"(\w)\w\1");
			return input.Lines()
				.Where(l => pair.IsMatch(l) && repeat.IsMatch(l))
				.Count();
		}
	}
}
