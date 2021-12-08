using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2021
{
	[Day(8)]
	public class Day8
	{
		[Part(1)]
		public object Part1(string input)
		{
			long count = 0;
			foreach (var line in input.Lines())
			{
				count += line.Split('|')
					.Last()
					.Split(' ', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
					.Where(s => s.Length == 2 || s.Length == 4 || s.Length == 3 || s.Length == 7)
					.Count();
			}
			return count;
		}

		[Part(2)]
		public object Part2(string input)
		{
			long sum = 0;
			foreach (var inputLine in input.Lines())
			{
				var a = inputLine.Split('|');
				var all = a[0].Split(' ', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
				var output = a[1].Split(' ', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);

				var numbers = new List<HashSet<char>> { null, null, null, null, null, null, null, null, null, null };
				var fives = new List<HashSet<char>>();
				var sixes = new List<HashSet<char>>();
				foreach (var part in all)
				{
					if (part.Length == 2)
					{
						numbers[1] = new HashSet<char>(part);
					}
					else if (part.Length == 3)
					{
						numbers[7] = new HashSet<char>(part);
					}
					else if (part.Length == 4)
					{
						numbers[4] = new HashSet<char>(part);
					}
					else if (part.Length == 7)
					{
						numbers[8] = new HashSet<char>(part);
					}
					else if (part.Length == 5)
					{
						fives.Add(new HashSet<char>(part));
					}
					else if (part.Length == 6)
					{
						sixes.Add(new HashSet<char>(part));
					}
				}

				foreach (var six in sixes)
				{
					if (!numbers[1].IsSubsetOf(six))
					{
						numbers[6] = six;
					}
					else if (numbers[4].IsSubsetOf(six))
					{
						numbers[9] = six;
					}
					else
					{
						numbers[0] = six;
					}
				}

				foreach (var five in fives)
				{
					if (five.IsSubsetOf(numbers[6]))
					{
						numbers[5] = five;
					}
					else if (five.IsSubsetOf(numbers[9]))
					{
						numbers[3] = five;
					}
					else
					{
						numbers[2] = five;
					}
				}

				sum += output.Aggregate(0, (c, p) => c * 10 + numbers.FindIndex(s => s.SetEquals(p)));
			}
			return sum;
		}
	}
}
