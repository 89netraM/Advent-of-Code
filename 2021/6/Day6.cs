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
			long[] fishes = new long[9];
			foreach (var kvp in ages)
			{
				fishes[kvp.Key] = kvp.Value;
			}

			int six = 6;
			for (int i = 0; i < 256; i++)
			{
				six++;
				fishes[MathM.Mod(six, fishes.Length)] += fishes[MathM.Mod(six + 2, fishes.Length)];
			}

			return fishes.Sum(static c => c);
		}
	}
}
