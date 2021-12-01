using System.Linq;
using AoC.Library;

namespace AoC.Year2021
{
	[Day(1)]
	public class Day1
	{
		[Part(1)]
		public object Part1(string input)
		{
			var nums = input.Lines().Select(long.Parse);
			long prev = nums.First();
			long larger = 0;
			foreach (var num in nums.Skip(1))
			{
				if (num > prev)
				{
					larger++;
				}
				prev = num;
			}
			return larger;
		}

		[Part(2)]
		public object Part2(string input)
		{
			var nums = input.Lines().Select(long.Parse);
			var windowNums = nums.Zip(nums.Skip(1).Zip(nums.Skip(2), (a, b) => a + b), (a, b) => a + b);
			long prev = windowNums.First();
			long larger = 0;
			foreach (var num in windowNums.Skip(1))
			{
				if (num > prev)
				{
					larger++;
				}
				prev = num;
			}
			return larger;
		}
	}
}
