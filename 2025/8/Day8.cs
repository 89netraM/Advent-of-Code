using System;
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
		return circuits.Product();
	}

	[Part(2)]
	public object Part2(string input)
	{
		var junctions = Parse(input);
		var pairs = CalculateDistances(junctions, junctions.Count * (junctions.Count - 1) / 2).ToImmutableArray();
		var (a, b) = FindLastMerge(junctions, pairs);
		return a.X * b.X;
	}

#if GAME
	[Part(0)]
	public object Part0(string input)
	{
		var junctions = Parse(input);
		var pairs = CalculateDistances(junctions).ToImmutableArray();
		using var game = new Day8Game(junctions, pairs);
		game.Run();
		return null;
	}
#endif

	private static IImmutableList<Vector3> Parse(string input) =>
		input.Lines().Extract<Vector3>("^(.*?),(.*?),(.*?)$").ToImmutableArray();

	private static IEnumerable<(Vector3, Vector3)> CalculateDistances(
		IImmutableList<Vector3> junctions,
		int lightStringCount = 1000
	)
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
				n =>
				{
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

	private static (Vector3, Vector3) FindLastMerge(
		IImmutableList<Vector3> junctions,
		IImmutableList<(Vector3, Vector3)> pairs
	)
	{
		var circuits = new LinkedList<IImmutableSet<Vector3>>(junctions.Select(j => ImmutableHashSet.Create(j)));
		foreach (var (a, b) in pairs)
		{
			var (aNode, bNode) = FindContaining(a, b);
			if (aNode == bNode)
			{
				continue;
			}
			circuits.Remove(bNode);
			if (circuits.Count is 1)
			{
				return (a, b);
			}
			aNode.Value = aNode.Value.Union(bNode.Value);
		}

		throw new Exception("Cannot connect all junctions.");

		(LinkedListNode<IImmutableSet<Vector3>>, LinkedListNode<IImmutableSet<Vector3>>) FindContaining(
			Vector3 a,
			Vector3 b
		)
		{
			LinkedListNode<IImmutableSet<Vector3>> aNode = null!,
				bNode = null!;
			for (var node = circuits.First; node is not null; node = node.Next)
			{
				if (node.Value.Contains(a))
				{
					aNode = node;
				}
				if (node.Value.Contains(b))
				{
					bNode = node;
				}
			}
			return (aNode, bNode);
		}
	}
}
