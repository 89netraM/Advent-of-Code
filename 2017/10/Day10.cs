using System;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(10)]
	public class Day10
	{
		[Part(1)]
		public object Part1(string input)
		{
			int[] lengths = input.Split(',').Select(Int32.Parse).ToArray();
			CircularArray array = new CircularArray(256);

			var currPos = 0;
			int skipSize = 0;
			foreach (int length in lengths)
			{
				array.ReverseSection(currPos, length);
				currPos = MathM.Mod(currPos + length + skipSize, array.Length);
				skipSize++;
			}

			return array[0] * array[1];
		}

		[Part(2)]
		public object Part2(string input)
		{
			return String.Concat(KnotHash.Calculate(input).Select(static x => $"{x:X2}".ToLower()));
		}
	}
}
