using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;
using RegExtract;

namespace AoC.Year2021
{
	[Day(14)]
	public class Day14
	{
		[Part(1)]
		public object Part1(string input)
		{
			var split = input.Split("\n\n");
			var polymer = split[0].ToList();
			var rules = split[1].Lines().Extract<(char, char, char)>(@"(\w)(\w) -> (\w)").ToDictionary(x => (x.Item1, x.Item2), x => x.Item3);

			var nextPolymer = new List<char>();
			for (int i = 0; i < 10; i++)
			{
				for (int j = 0; j < polymer.Count - 1; j++)
				{
					nextPolymer.Add(polymer[j]);
					var pair = (polymer[j], polymer[j + 1]);
					if (rules.TryGetValue(pair, out var middle))
					{
						nextPolymer.Add(middle);
					}
				}
				nextPolymer.Add(polymer[^1]);
				(polymer, nextPolymer) = (nextPolymer, polymer);
				nextPolymer.Clear();
			}

			var groups = polymer.GroupBy(Id).ToList();
			return groups.Max(x => x.Count()) - groups.Min(x => x.Count());
		}

		[Part(2)]
		public object Part2(string input)
		{
			var split = input.Split("\n\n");
			var rules = split[1].Lines().Extract<(char, char, char)>(@"(\w)(\w) -> (\w)").ToDictionary(x => (x.Item1, x.Item2), x => x.Item3);
			var pairCount = split[0].Zip(split[0].Skip(1), (a, b) => (a, b)).ToCounter();
			var nextPairCount = new Dictionary<(char, char), long>();

			for (int i = 0; i < 40; i++)
			{
				foreach (var kvp in pairCount)
				{
					if (rules.TryGetValue(kvp.Key, out var middle))
					{
						nextPairCount.Increase((kvp.Key.a, middle), kvp.Value);
						nextPairCount.Increase((middle, kvp.Key.b), kvp.Value);
					}
				}
				(pairCount, nextPairCount) = (nextPairCount, pairCount);
				nextPairCount.Clear();
			}

			var count = new Dictionary<char, long>();
			foreach (var kvp in pairCount)
			{
				count.Increase(kvp.Key.a, kvp.Value);
			}
			count.Increase(split[0][^1]);
			var common = count.MostCommon().ToArray();
			return common[0].count - common[^1].count;
		}
	}
}
