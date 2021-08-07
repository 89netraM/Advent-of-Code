using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(1)]
	public class Day1
	{
		[Part(1)]
		public object Part1(string input)
		{
			int[] numbers = input.Select(static c => c - '0').ToArray();
			return numbers.Zip(numbers.Concat(numbers).Skip(1)).Sum(static p => p.First == p.Second ? p.First : 0);
		}

		[Part(2)]
		public object Part2(string input)
		{
			int[] numbers = input.Select(static c => c - '0').ToArray();
			return numbers.Zip(numbers.Concat(numbers).Skip(numbers.Length / 2)).Sum(static p => p.First == p.Second ? p.First : 0);
		}
	}
}
