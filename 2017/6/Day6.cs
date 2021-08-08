using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(6)]
	public class Day6
	{
		[Part(1)]
		public object Part1(string input)
		{
			int[] banks = input.Words().Select(Int32.Parse).ToArray();
			HashSet<string> seen = new HashSet<string>();
			seen.Add(String.Join(" ", banks));

			for (int i = 0; true; i++)
			{
				Cycle(banks);
				if (!seen.Add(String.Join(" ", banks)))
				{
					return i + 1;
				}
			}
		}

		private static void Cycle(int[] banks)
		{
			int index = banks.Select(static (v, i) => (v, i)).OrderByDescending(static p => p.v).First().i;
			int blocks = banks[index];
			banks[index] = 0;
			while (blocks > 0)
			{
				index = MathM.Mod(index + 1, banks.Length);
				banks[index]++;
				blocks--;
			}
		}

		[Part(2)]
		public object Part2(string input)
		{
			int[] banks = input.Words().Select(Int32.Parse).ToArray();
			HashSet<string> seen = new HashSet<string>();
			int? first = null;
			seen.Add(String.Join(" ", banks));

			for (int i = 0; true; i++)
			{
				Cycle(banks);
				if (!seen.Add(String.Join(" ", banks)))
				{
					if (first is int f)
					{
						return i - f - 1;
					}
					else
					{
						first = i;
						seen.Clear();
					}
				}
			}
		}
	}
}
