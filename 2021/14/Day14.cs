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
			var pairCount = new Dictionary<(char a, char b), long>();
			foreach (var pair in split[0].Zip(split[0].Skip(1), Curry<char, char, (char, char)>(Id)))
			{
				if (pairCount.ContainsKey(pair))
				{
					pairCount[pair] += 1;
				}
				else
				{
					pairCount[pair] = 1;
				}
			}
			var nextPairCount = new Dictionary<(char, char), long>();

			for (int i = 0; i < 40; i++)
			{
				foreach (var kvp in pairCount)
				{
					if (rules.TryGetValue(kvp.Key, out var middle))
					{
						if (nextPairCount.ContainsKey((kvp.Key.a, middle)))
						{
							nextPairCount[(kvp.Key.a, middle)] += kvp.Value;
						}
						else
						{
							nextPairCount[(kvp.Key.a, middle)] = kvp.Value;
						}
						if (nextPairCount.ContainsKey((middle, kvp.Key.b)))
						{
							nextPairCount[(middle, kvp.Key.b)] += kvp.Value;
						}
						else
						{
							nextPairCount[(middle, kvp.Key.b)] = kvp.Value;
						}
					}
				}
				(pairCount, nextPairCount) = (nextPairCount, pairCount);
				nextPairCount.Clear();
			}

			var count = new Dictionary<char, long>();
			foreach (var kvp in pairCount)
			{
				if (count.ContainsKey(kvp.Key.a))
				{
					count[kvp.Key.a] += kvp.Value;
				}
				else
				{
					count[kvp.Key.a] = kvp.Value;
				}
			}
			if (count.ContainsKey(split[0][^1]))
			{
				count[split[0][^1]] += 1;
			}
			else
			{
				count[split[0][^1]] = 1;
			}
			return count.Max(x => x.Value) - count.Min(x => x.Value);
		}
	}
}
