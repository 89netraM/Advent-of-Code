using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using AoC.Library;

namespace AoC.Year2016;

[Day(11)]
public class Day11
{
	private static readonly Regex thingMatcher = new Regex(@"a(?:n)? (\w+)(?:-compatible)? (generator|microchip)");

	[Part(1)]
	public object Part1(string input)
	{
		var area = new ContainmentArea();
		foreach (var (i, line) in input.Lines().Enumerate())
		{
			foreach (Match match in thingMatcher.Matches(line))
			{
				var element = Enum.Parse<Element>(match.Groups[1].Value, true);
				var kind = Enum.Parse<Kind>(match.Groups[2].Value, true);
				area = area.Add(i, element, kind);
			}
		}

		Print((0, area));

		BFS.Search(
			(0, area),
			GetNexts,
			IsDone,
			out var path);

		foreach (var (i, (step, d)) in path.Enumerate())
		{
			Console.WriteLine($"Step {i + 1} ({d})");
			Print(step);
		}

		return path.Count();
	}

	[Part(2)]
	public object Part2(string input)
	{
		var lines = input.Lines().ToArray();
		lines[0] += " an elerium generator. an elerium-compatible microchip. a dilithium generator. a dilithium-compatible microchip.";
		return Part1(String.Join(Environment.NewLine, lines));
	}

	private IEnumerable<((int, ContainmentArea), long)> GetNexts((int e, ContainmentArea) current) =>
		GetNexts(current, current.e + 1, 1).Concat(GetNexts(current, current.e - 1, 2));
	private IEnumerable<((int, ContainmentArea), long)> GetNexts((int, ContainmentArea) current, int nextElevator, long cost)
	{
		var (elevator, area) = current;
		if (0 <= nextElevator && nextElevator < 4)
		{
			foreach (var thing in area.ThingsOfFloor(elevator))
			{
				var next = area.Move(thing, elevator, nextElevator);
				if (next.IsValid(elevator) && next.IsValid(nextElevator))
				{
					yield return ((nextElevator, next), cost);
				}

				foreach (var otherThing in area.ThingsOfFloor(elevator))
				{
					if (thing != otherThing)
					{
						var nextNext = next.Move(otherThing, elevator, nextElevator);
						if (nextNext.IsValid(elevator) && nextNext.IsValid(nextElevator))
						{
							yield return ((nextElevator, nextNext), cost);
						}
					}
				}
			}
		}
	}

	private bool IsDone((int, ContainmentArea area) current) => current.area.IsDone();

	private void Print((int, ContainmentArea) current)
	{
		var (elevator, area) = current;
		for (int i = 3; i >= 0; i--)
		{
			Console.Write($"F{i + 1} {(i == elevator ? 'E' : '.')} ");
			var floor = area[i];
			for (int j = 0; j < ContainmentArea.ElementCount * ContainmentArea.KindCount; j++)
			{
				if ((floor & 1) != 0)
				{
					var element = (Element)(j / ContainmentArea.KindCount);
					var kind = (Kind)(j - (int)element * ContainmentArea.KindCount);
					Console.Write($" {element.ToString()[0]}{kind.ToString()[0]}");
				}
				else
				{
					Console.Write(" . ");
				}
				floor >>= 1;
			}
			Console.WriteLine();
		}
	}

	private readonly record struct ContainmentArea(ulong Map)
	{
		public const int ElementCount = 7;
		public const int KindCount = 2;

		public ulong this[int i] => Map >> ElementCount * KindCount * i;

		public ContainmentArea Add(int index, Element element, Kind kind) =>
			new ContainmentArea(Map | (1UL << ((int)kind + (int)element * KindCount + index * ElementCount * KindCount)));

		public IEnumerable<ulong> ThingsOfFloor(int index)
		{
			var floor = this[index];
			for (int i = 0; i < ElementCount * KindCount; i++)
			{
				var thing = floor & (1UL << i);
				if (thing != 0)
				{
					yield return thing;
				}
			}
		}

		public ContainmentArea Move(ulong thing, int from, int to) =>
			new ContainmentArea(Map & ~(thing << from * ElementCount * KindCount) | (thing << to * ElementCount * KindCount));

		public bool IsValid(int index)
		{
			var floor = this[index];
			var hasGenerator = false;
			var hasUnprotectedChip = false;
			for (int i = 0; i < ElementCount; i++)
			{
				switch (floor & 0b11)
				{
					case 0b11:
					case 0b01:
						hasGenerator = true;
						break;
					case 0b10:
						hasUnprotectedChip = true;
						break;
					case 0b00:
						break;
				}

				if (hasGenerator && hasUnprotectedChip)
				{
					return false;
				}

				floor >>= KindCount;
			}
			return true;
		}

		public bool IsDone() => (Map & 0b111111111111111111111111111111111111111111) == 0;
	}

	private enum Element
	{
		Plutonium = 0,
		Promethium = 1,
		Cobalt = 2,
		Curium = 3,
		Ruthenium = 4,
		Elerium = 5,
		Dilithium = 6,
	}

	private enum Kind
	{
		Generator = 0,
		Microchip = 1,
	}
}
