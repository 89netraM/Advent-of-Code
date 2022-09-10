using System;
using System.Linq;
using AoC.Library;

namespace AoC.Year2015
{
	[Day(20)]
	public class Day20
	{
		[Part(1)]
		public object Part1(string input)
		{
			int target = Int32.Parse(input);
			int[] house = new int[target / 10];

			for (int i = 1; i < target / 10; i++)
			{
				for (int j = i; j < target / 10; j += i)
				{
					house[j] += i * 10;
				}
			}

			return house.Select((v, i) => (v, i)).First(p => p.v >= target).i;
		}

		[Part(2)]
		public object Part2(string input)
		{
			int target = Int32.Parse(input);
			int[] house = new int[target / 11];

			for (int i = 1; i < target / 11; i++)
			{
				for (int j = i; j < target / 11; j += i)
				{
					if (j / i <= 50)
					{
						house[j] += i * 11;
					}
				}
			}

			return house.Select((v, i) => (v, i)).First(p => p.v >= target).i;
		}
	}
}
