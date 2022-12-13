using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2022;

[Day(13)]
public class Day13
{
	[Part(1)]
	public object Part1(string input) =>
		input.Split("\n\n")
			.Select((s, i) => (s, i))
			.Where(p => Smaller(p.s))
			.Sum(p => p.i + 1);

	[Part(2)]
	public object Part2(string input)
	{
		var two = new List(new List<IItem> { new Item(2) });
		var six = new List(new List<IItem> { new Item(6) });
		var packets = input.Split('\n', StringSplitOptions.RemoveEmptyEntries)
			.Select(l => (IItem)new List(IItem.Parse(l)))
			.Append(two)
			.Append(six)
			.ToList();
		packets.Sort((a, b) => Smaller((a, b)));
		var twoIndex = packets.IndexOf(two) + 1;
		var sixIndex = packets.IndexOf(six) + 1;
		return twoIndex * sixIndex;
	}

	private bool Smaller(string input)
	{
		var lines = input.Lines();
		var a = new List(IItem.Parse(lines[0]));
		var b = new List(IItem.Parse(lines[1]));
		return Smaller((a, b)) <= 0;
	}
	private int Smaller((IItem, IItem) pair) =>
		pair switch
		{
			(Item a, Item b) => a.Value.CompareTo(b.Value),
			(Item a, List b) => Smaller((new List(new List<IItem> { a }), b)),
			(List a, Item b) => Smaller((a, new List(new List<IItem> { b }))),
			(List a, List b) => a.Items.Zip(b.Items).Aggregate(0, (r, p) => r != 0 ? r : Smaller(p)) switch
			{
				0 => a.Items.Count.CompareTo(b.Items.Count),
				int n => n,
			},
			_ => throw new Exception(),
		};

	interface IItem
	{
		public static List<IItem> Parse(string input) =>
			Parse(input.AsMemory().Slice(1, input.Length - 2));
		public static List<IItem> Parse(ReadOnlyMemory<char> input)
		{
			var items = new List<IItem>();
			var level = new Stack<int>();
			int? prev = null;
			for (int i = 0; i < input.Length; i++)
			{
				var c = input.Span[i];
				if (c == '[')
				{
					level.Push(i + 1);
				}
				else if (c == ',')
				{
					if (level.Count == 0 && prev is int p)
					{
						items.Add(new Item(long.Parse(input.Span.Slice(p, i - p))));
						prev = null;
					}
				}
				else if (c == ']')
				{
					var start = level.Pop();
					if (level.Count == 0)
					{
						items.Add(new List(Parse(input.Slice(start, i - start))));
					}
				}
				else if (prev is null)
				{
					if (level.Count == 0)
					{
						prev = i;
					}
				}
			}
			{
				if (prev is int p)
				{
					items.Add(new Item(long.Parse(input.Span.Slice(p, input.Length - p))));
				}
			}
			return items;
		}
	}
	record Item(long Value) : IItem;
	record List(List<IItem> Items) : IItem;
}
