using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2022;

[Day(20)]
public class Day20
{
	[Part(1)]
	public long Part1(string input)
	{
		var list = input.Lines().Select((l, i) => new Num(long.Parse(l) * 811589153L, i)).ToArray();
		var order = list.ToArray();
		Mix(list, order);
		return SumGroveCoordinates(list);
	}

	[Part(2)]
	public long Part2(string input)
	{
		var list = input.Lines().Select((l, i) => new Num(long.Parse(l) * 811589153L, i)).ToArray();
		var order = list.ToArray();
		for (int i = 0; i < 10; i++)
			Mix(list, order);
		return SumGroveCoordinates(list);
	}

	private void Mix(Num[] list, IEnumerable<Num> order)
	{
		foreach (var num in order)
		{
			foreach (var other in list)
				if (other.Index > num.Index)
					other.Index--;
			num.Index = MathM.Mod(num.Index + num.Value, list.Length - 1);
			foreach (var other in list)
				if (other.Index >= num.Index && other != num)
					other.Index++;
		}
	}

	private long SumGroveCoordinates(Num[] list)
	{
		Array.Sort(list);
		var zero = Array.FindIndex(list, n => n.Value == 0);
		return list[(zero + 1000) % list.Length].Value +
			list[(zero + 2000) % list.Length].Value +
			list[(zero + 3000) % list.Length].Value;
	}

	private class Num : IComparable<Num>
	{
		public long Value { get; set; }
		public long Index { get; set; }

		public Num(long value, long index) =>
			(Value, Index) = (value, index);

		public int CompareTo(Num other) =>
			Index.CompareTo(other.Index);
	}

	[Part(3)]
	public long Part3(string input)
	{
		var list = new LinkedList<long>(input.Lines().Select(long.Parse));
		var order = Nodes(list);
		Mix(list, order);
		return SumGroveCoordinates(list);
	}

	[Part(4)]
	public long Part4(string input)
	{
		var list = new LinkedList<long>(input.Lines().Select(l => long.Parse(l) * 811589153L));
		var order = Nodes(list);
		for (int i = 0; i < 10; i++)
			Mix(list, order);
		return SumGroveCoordinates(list);
	}

	private List<LinkedListNode<long>> Nodes(LinkedList<long> list)
	{
		var nodes = new List<LinkedListNode<long>>(list.Count);
		for (var node = list.First; node is not null; node = node.Next)
			nodes.Add(node);
		return nodes;
	}

	private void Mix(LinkedList<long> list, IEnumerable<LinkedListNode<long>> order)
	{
		foreach (var node in order)
		{
			var before = node.Previous ?? list.Last;
			list.Remove(node);
			var moves = MathM.Mod(node.Value, list.Count);
			for (long i = 0; i < moves; i++)
				before = before.Next ?? list.First;
			list.AddAfter(before, node);
		}
	}

	private long SumGroveCoordinates(LinkedList<long> list)
	{
		var node = list.First;
		for (; node is not null && node.Value != 0; node = node.Next) ;

		long sum = 0;
		for (int i = 0; i <= 3000; i++)
		{
			if (i == 1000 || i == 2000 || i == 3000)
				sum += node.Value;
			node = node.Next ?? list.First;
		}
		return sum;
	}
}
