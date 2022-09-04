using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;
using System.Collections.Immutable;

namespace AoC.Year2015
{
	[Day(13)]
	public class Day13
	{
		[Part(1)]
		public object Part1(string input)
		{
			var (people, pairCost) = ParsePeople(input);
			return CalculateCosts(people, pairCost).Max();
		}

		[Part(2)]
		public object Part2(string input)
		{
			var (people, pairCost) = ParsePeople(input);
			people.Add("Mårten");
			return CalculateCosts(people, pairCost).Max();
		}

		private (HashSet<string>, Dictionary<UOP<string>, long>) ParsePeople(string input)
		{
			var people = new HashSet<string>();
			var pairCost = new Dictionary<UOP<string>, long>();
			foreach (var (a, gl, hp, b) in input.Lines().Extract<(string, GainLose, long, string)>(@"(\w+) would (\w+) (\d+) happiness units by sitting next to (\w+)"))
			{
				var hpPoint = (long)gl * hp;
				pairCost.AddOrUpdate(UOP.New(a, b), hpPoint, old => old + hpPoint);
				people.Add(a);
				people.Add(b);
			}
			return (people, pairCost);
		}

		private IEnumerable<long> CalculateCosts(IReadOnlySet<string> people, IReadOnlyDictionary<UOP<string>, long> pairCost)
		{
			var start = people.First();
			var initialCostToBuilder = ImmutableDictionary.CreateBuilder<string, long>();
			initialCostToBuilder.Add(start, 0);
			var toVisit = new Queue<(string, IImmutableDictionary<string, long>)>();
			toVisit.Enqueue((start, initialCostToBuilder.ToImmutable()));

			while (toVisit.Count > 0)
			{
				var (current, costTo) = toVisit.Dequeue();
				foreach (var next in people.Where(p => p != current))
				{
					if (!costTo.ContainsKey(next))
					{
						var nextCost = costTo[current] + pairCost.GetValueOrDefault(UOP.New(current, next), 0);
						var nextCostTo = costTo.Add(next, nextCost);
						if (nextCostTo.Count == people.Count)
						{
							yield return nextCost + pairCost.GetValueOrDefault(UOP.New(start, next), 0);
						}
						else
						{
							toVisit.Enqueue((next, nextCostTo));
						}
					}
				}
			}
		}

		private enum GainLose : long
		{
			gain = 1,
			lose = -1,
		}
	}
}
