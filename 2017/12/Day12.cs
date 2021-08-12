using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(12)]
	public class Day12
	{
		[Part(1)]
		public object Part1(string input)
		{
			var dict = input.Lines()
				.Select(Parsing.PatternParser<string, string>(@"(\d+) <-> (.*)"))
				.ToDictionary(static p => p?.Item1, static p => p?.Item2.Split(", "));
			HashSet<string> inZero = new HashSet<string>();
			inZero.Add("0");

			bool change = true;
			while (change)
			{
				change = false;
				foreach (string k in inZero.ToList())
				{
					var list = dict[k];
					foreach (var v in list)
					{
						change = inZero.Add(v) || change;
					}
				}
			}

			return inZero.Count;
		}

		[Part(2)]
		public object Part2(string input)
		{
			var dict = input.Lines()
				.Select(Parsing.PatternParser<string, string>(@"(\d+) <-> (.*)"))
				.ToDictionary(static p => p?.Item1, static p => p?.Item2.Split(", "));

			int groups = 0;
			while (dict.Count > 0)
			{
				HashSet<string> inGroup = new HashSet<string>();
				inGroup.Add(dict.First().Key);

				bool change = true;
				while (change)
				{
					change = false;
					foreach (string k in inGroup.ToList())
					{
						var list = dict[k];
						foreach (var v in list)
						{
							change = inGroup.Add(v) || change;
						}
					}
				}

				foreach (var k in inGroup)
				{
					dict.Remove(k);
				}

				groups++;
			}

			return groups;
		}
	}
}
