using System;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(5)]
	public class Day5
	{
		[Part(1)]
		public object Part1(string input)
		{
			int[] jumps = input.Lines().Select(Int32.Parse).ToArray();

			int i = 0;
			int pc = 0;
			while (0 <= pc && pc < jumps.Length)
			{
				int ogPc = pc;
				pc += jumps[pc];
				jumps[ogPc]++;
				i++;
			}
			return i;
		}

		[Part(2)]
		public object Part2(string input)
		{
			int[] jumps = input.Lines().Select(Int32.Parse).ToArray();

			int i = 0;
			int pc = 0;
			while (0 <= pc && pc < jumps.Length)
			{
				int ogPc = pc;
				pc += jumps[pc];
				jumps[ogPc] += jumps[ogPc] >= 3 ? -1 : 1;
				i++;
			}
			return i;
		}
	}
}
