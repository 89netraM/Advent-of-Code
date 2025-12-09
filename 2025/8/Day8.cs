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
		var pairs = CalculateDistances(junctions).ToImmutableArray();
		var circuits = CalculateCircuits(pairs);
		// using var game = new Day8Game(junctions, pairs, circuits.ToImmutableArray());
		// game.Run();
		return circuits.Product();
	}

	private static IImmutableList<Vector3> Parse(string input) =>
		input.Lines().Extract<Vector3>("^(.*?),(.*?),(.*?)$").ToImmutableArray();

	private static IEnumerable<(Vector3, Vector3)> CalculateDistances(IImmutableList<Vector3> junctions, int lightStringCount = 1000)
	{
		var distances = new SortedList<double, (Vector3, Vector3)>(lightStringCount);
		for (var i = 0; i < junctions.Count - 1; i++)
		for (var j = i + 1; j < junctions.Count; j++)
		{
			var distance = junctions[i].Distance(junctions[j]);
			if (distances.Count == lightStringCount)
			{
				if (distances.GetKeyAtIndex(lightStringCount - 1) > distance)
				{
					distances.RemoveAt(lightStringCount - 1);
					distances.Add(distance, (junctions[i], junctions[j]));
				}
			}
			else
			{
				distances.Add(distance, (junctions[i], junctions[j]));
			}
		}
		return distances.Select(kvp => kvp.Value);
	}

	private static IEnumerable<long> CalculateCircuits(IImmutableList<(Vector3, Vector3)> pairs, int circuitCount = 3)
	{
		var map = new Dictionary<Vector3, IImmutableList<Vector3>>(pairs.Count * 2);
		foreach (var (a, b) in pairs)
		{
			map[a] = (map.GetValueOrDefault(a) ?? ImmutableList<Vector3>.Empty).Add(b);
			map[b] = (map.GetValueOrDefault(b) ?? ImmutableList<Vector3>.Empty).Add(a);
		}

		var nodes = map.Keys.ToHashSet();
		var circuits = new PriorityQueue<long, long>(circuitCount + 1);

		while (nodes.Count > 0)
		{
			var startNode = nodes.First();
			nodes.Remove(startNode);
			var size = 1L;
			BFS.Search(
				startNode,
				n => map[n],
				_ => false,
				out _,
				n => {
					nodes.Remove(n);
					size++;
				}
			);
			circuits.Enqueue(size, size);
			while (circuits.Count > circuitCount)
			{
				circuits.Dequeue();
			}
		}

		return circuits.UnorderedItems.Select(p => p.Element);
	}
}
