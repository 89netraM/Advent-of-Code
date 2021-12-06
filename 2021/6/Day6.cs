using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;

namespace AoC.Year2021
{
	[Day(6)]
	public class Day6
	{
		[Part(1)]
		public object Part1(string input)
		{
			var ages = input.Split(',').Select(long.Parse).ToList();

			for (var i = 0; i < 80; i++)
			{
				var ogCount = ages.Count;
				for (var j = 0; j < ogCount; j++)
				{
					if (ages[j] == 0)
					{
						ages[j] = 6;
						ages.Add(8);
					}
					else
					{
						ages[j]--;
					}
				}
			}

			return ages.Count;
		}

		[Part(2)]
		public object Part2(string input)
		{
			var ages = input.Split(',').Select(long.Parse).GroupBy(Id).ToDictionary(static x => x.Key, static x => (long)x.Count());
			for (long i = 0L; i <= 8L; i++)
			{
				if (!ages.ContainsKey(i))
				{
					ages.Add(i, 0L);
				}
			}

			var ages2 = new Dictionary<long, long>();
			for (int i = 0; i < 256; i++)
			{
				for (long j = 0L; j <= 8L; j++)
				{
					ages2[j] = 0L;
				}

				for (long j = 8L; j >= 0L; j--)
				{
					if (j == 0)
					{
						ages2[8] += ages[j];
						ages2[6] += ages[j];
					}
					else
					{
						ages2[j - 1] += ages[j];
					}
				}

				(ages, ages2) = (ages2, ages);
			}

			return ages.Sum(static x => x.Value);
		}
	}
}
