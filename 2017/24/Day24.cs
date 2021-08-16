using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(24)]
	public class Day24
	{
		[Part(1)]
		public object Part1(string input)
		{
			var components = input.Lines()
				.Select(Parsing.PatternParser<long, long>(@"(\d+)/(\d+)"))
				.Select(static p => ((long, long))p)
				.ToArray();
			var zeroes = components.Where(static p => p.Item1 == 0 || p.Item2 == 0);

			long maxStrength = 0;
			foreach (var zero in zeroes)
			{
				long value = 0;
				HashSet<(long, long)> taken = new HashSet<(long, long)> { zero };

				void AddDeeper(long last)
				{
					foreach (var component in components)
					{
						if ((component.Item1 == last || component.Item2 == last) && taken.Add(component))
						{
							value += component.Item1 + component.Item2;
							maxStrength = Math.Max(maxStrength, value);
							AddDeeper(component.Item1 == last ? component.Item2 : component.Item1);
							value -= component.Item1 + component.Item2;
							taken.Remove(component);
						}
					}
				}

				AddDeeper(value);
			}

			return maxStrength;
		}

		[Part(2)]
		public object Part2(string input)
		{
			var components = input.Lines()
				.Select(Parsing.PatternParser<long, long>(@"(\d+)/(\d+)"))
				.Select(static p => ((long, long))p)
				.ToArray();
			var zeroes = components.Where(static p => p.Item1 == 0 || p.Item2 == 0);

			long longest = 0;
			long maxStrength = 0;
			foreach (var zero in zeroes)
			{
				long value = zero.Item1 + zero.Item2;
				long length = 1;
				HashSet<(long, long)> taken = new HashSet<(long, long)> { zero };

				void AddDeeper(long last)
				{
					foreach (var component in components)
					{
						if ((component.Item1 == last || component.Item2 == last) && taken.Add(component))
						{
							length++;
							value += component.Item1 + component.Item2;
							if (length >= longest)
							{
								maxStrength = Math.Max(maxStrength, value);
								longest = length;
							}
							AddDeeper(component.Item1 == last ? component.Item2 : component.Item1);
							value -= component.Item1 + component.Item2;
							taken.Remove(component);
							length--;
						}
					}
				}

				AddDeeper(value);
			}

			return maxStrength;
		}
	}
}
