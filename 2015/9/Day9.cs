using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2015
{
	[Day(9)]
	public class Day9
	{
		[Part(1)]
		public long Part1(string input) =>
			CalculateAllPathCosts(input)
				.Min();

		[Part(2)]
		public long Part2(string input) =>
			CalculateAllPathCosts(input)
				.Max();

		private IEnumerable<long> CalculateAllPathCosts(string input)
		{
			var (costs, neighbors) = ParseCostsAndNeighbors(input);
			return neighbors.Keys
				.AsParallel()
				.SelectMany(node => CalculateCostFrom(costs, neighbors, node));
		}

		private (IReadOnlyDictionary<UOP<string>, long>, IReadOnlyDictionary<string, IEnumerable<string>>) ParseCostsAndNeighbors(string input)
		{
			var costs = new Dictionary<UOP<string>, long>();
			var neighbors = new Dictionary<string, IEnumerable<string>>();
			foreach (var (from, to, cost) in input.Lines().Extract<(string, string, long)>(@"^(.*?) to (.*?) = (.*)$"))
			{
				costs.Add(UOP.New(from, to), cost);
				neighbors.AddOrUpdate(from, () => new List<string> { to }, neighbors => neighbors.Append(to));
				neighbors.AddOrUpdate(to, () => new List<string> { from }, neighbors => neighbors.Append(from));
			}
			return (costs, neighbors);
		}

		private IEnumerable<long> CalculateCostFrom(IReadOnlyDictionary<UOP<string>, long> costs, IReadOnlyDictionary<string, IEnumerable<string>> neighbors, string start)
		{
			var initialCostToBuilder = ImmutableDictionary.CreateBuilder<string, long>();
			initialCostToBuilder.Add(start, 0);
			var toVisit = new Queue<(string, IImmutableDictionary<string, long>)>();
			toVisit.Enqueue((start, initialCostToBuilder.ToImmutable()));

			while (toVisit.Count > 0)
			{
				var (current, costTo) = toVisit.Dequeue();
				foreach (var next in neighbors[current])
				{
					if (!costTo.ContainsKey(next))
					{
						var nextCost = costTo[current] + costs[UOP.New(current, next)];
						var nextCostTo = costTo.Add(next, nextCost);
						if (nextCostTo.Count == neighbors.Count)
						{
							yield return nextCost;
						}
						else
						{
							toVisit.Enqueue((next, nextCostTo));
						}
					}
				}
			}
		}
	}
}
