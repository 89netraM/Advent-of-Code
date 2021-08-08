using System;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(2)]
	public class Day2
	{
		[Part(1)]
		public object Part1(string input)
		{
			return input.Split('\n')
				.Select(static l =>
				{
					int[] numbers = l.Split('\t').Select(Int32.Parse).ToArray();
					return numbers.Max() - numbers.Min();
				})
				.Sum();
		}

		[Part(2)]
		public object Part2(string input)
		{
			return input.Split('\n')
				.Select(static l =>
				{
					int[] numbers = l.Split('\t').Select(Int32.Parse).ToArray();
					foreach (var n in numbers)
					{
						foreach (var m in numbers)
						{
							if (n != m && n % m == 0)
							{
								return n / m;
							}
						}
					}
					return 0;
				})
				.Sum();
		}
	}
}
