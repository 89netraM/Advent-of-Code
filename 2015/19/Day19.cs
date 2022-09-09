using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2015
{
	[Day(19)]
	public class Day19
	{
		[Part(1)]
		public object Part1(string input)
		{
			var split = input.Split("\n\n");
			var replacements = split[0].Lines().Extract<(string, string)>(@"^(.*?) => (.*)$").ToArray();
			var start = split[1].Trim();

			var outputs = new HashSet<string>();

			foreach (var (target, replacement) in replacements)
			{
				for (int i = start.IndexOf(target); i != -1; i = start.IndexOf(target, i + target.Length))
				{
					outputs.Add(start[0..i] + replacement + start[(i + target.Length)..]);
				}
			}

			return outputs.Count;
		}

		[Part(2)]
		public object Part2(string input)
		{
			var split = input.Split("\n\n");
			var replacements = split[0].Lines().Extract<(string, string)>(@"^(.*?) => (.*)$").ToArray();
			var goal = split[1].Trim();

			var target = goal;
			long count = 0;
			while (target != "e")
			{
				bool change = false;
				foreach (var (a, b) in replacements)
				{
					int i = target.IndexOf(b);
					if (i != -1)
					{
						target = target[0..i] + a + target[(i + b.Length)..];
						count++;
						change = true;
					}
				}
				if (!change)
				{
					target = goal;
					count = 0;
					Shuffle(replacements);
				}
			}
			return count;
		}

		public void Shuffle<T> (T[] array)
		{
			int n = array.Length;
			while (n > 1)
			{
				int k = Random.Shared.Next(n--);
				(array[n], array[k]) = (array[k], array[n]);
			}
		}
	}
}
