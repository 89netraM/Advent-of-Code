using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(4)]
	public class Day4
	{
		[Part(1)]
		public object Part1(string input)
		{
			return input.Split("\r\n")
				.Sum(static l =>
				{
					string[] words = l.Split(' ');
					return words.Length == words.Distinct().Count() ? 1 : 0;
				});
		}

		[Part(2)]
		public object Part2(string input)
		{
			return input.Split("\r\n")
				.Sum(static l =>
				{
					HashSet<char>[] words = l.Split(' ').Select(static w => new HashSet<char>(w)).ToArray();
					for (int i = 0; i < words.Length; i++)
					{
						for (int j = i + 1; j < words.Length; j++)
						{
							if (words[i].SetEquals(words[j]))
							{
								return 0;
							}
						}
					}
					return 1;
				});
		}
	}
}
