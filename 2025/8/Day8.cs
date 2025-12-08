using System.Collections.Generic;
using System.Collections.Immutable;
using System.Data;
using System.Linq;
using AoC.Library;
using RegExtract;
using static AoC.Library.Functional;

namespace AoC.Year2025.Day8;

[Day(8)]
public class Day8
{
	[Part(1)]
	public object Part1(string input)
	{
		var junctions = Parse(input);
		var pairs = CalculateDistances(junctions).Take(10).ToImmutableArray();
		var (sizes, circuits) = CalculateCircuits(pairs).Take(3).UnZip(Id);
		using var game = new Day8Game(junctions, pairs, circuits.ToImmutableArray());
		game.Run();
		return sizes.Select(i => (long)i).Product();
	}

	private static IImmutableList<Vector3> Parse(string input) =>
		input.Lines().Extract<Vector3>("^(.*?),(.*?),(.*?)$").ToImmutableArray();

	private static IEnumerable<(Vector3, Vector3)> CalculateDistances(IImmutableList<Vector3> junctions)
	{
		var distances = new SortedList<double, (Vector3, Vector3)>(1000);
		for (var i = 0; i < junctions.Count - 1; i++)
		for (var j = i + 1; j < junctions.Count; j++)
		{
			distances.Add(junctions[i].Distance(junctions[j]), (junctions[i], junctions[j]));
		}
		return distances.Select(kvp => kvp.Value);
	}

	private static IEnumerable<(int, IImmutableList<int>)> CalculateCircuits(IImmutableList<(Vector3, Vector3)> pairs)
	{
		var circuits = new LinkedList<ImmutableList<int>>(
			Enumerable.Range(0, pairs.Count).Select(ImmutableList.Create)
		);

		Update:
		for (var i = circuits.First; i?.Next is not null; i = i.Next)
		for (var j = i.Next; j is not null; j = j.Next)
		{
			foreach (var a in i.Value)
			foreach (var b in j.Value)
			{
				if (
					pairs[a].Item1 == pairs[b].Item1
					|| pairs[a].Item1 == pairs[b].Item2
					|| pairs[a].Item2 == pairs[b].Item1
					|| pairs[a].Item2 == pairs[b].Item2
				)
				{
					i.Value = i.Value.AddRange(j.Value);
					circuits.Remove(j);
					goto Update;
				}
			}
		}
		return circuits
			.Select(c =>
				(
					c.SelectMany<int, Vector3>(i => [pairs[i].Item1, pairs[i].Item2]).Distinct().Count(),
					(IImmutableList<int>)c
				)
			)
			.OrderByDescending(p => p.Item1);
	}
}
