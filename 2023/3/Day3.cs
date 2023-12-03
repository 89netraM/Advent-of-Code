using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2023;

[Day(3)]
public class Day3
{
	[Part(1)]
	public object Part1(string input)
	{
		var (numbers, symbols) = Parse(input);
		return numbers
			.Where(p => p.coords
				.SelectMany(c => c.NeighborsMoore())
				.Distinct()
				.Any(symbols.ContainsKey))
			.Sum(p => p.number);
	}

	[Part(2)]
	public object Part2(string input)
	{
		var (numbers, symbols) = Parse(input);
		return symbols.Where(p => p.Value == '*')
			.Select(p => p.Key.NeighborsMoore().ToHashSet())
			.Select(neighbors => numbers.Where(number => number.coords.Any(neighbors.Contains)).ToList())
			.Where(l => l.Count == 2)
			.Select(l => l[0].number * l[1].number)
			.Sum();
	}
	private static (List<(long number, ISet<Vector2> coords)>, IDictionary<Vector2, char>) Parse(string input)
	{
		var charMap = input.ToMap().Where(p => p.Value != '.').ToDictionary();
		var numbers = new List<(long n, ISet<Vector2> coords)>();
		var symbols = new Dictionary<Vector2, char>();
		foreach (var (coord, c) in charMap)
		{
			if (char.IsDigit(c))
			{
				if (numbers.Count > 0 && numbers[^1].coords.Contains(coord + Vector2.Left))
				{
					var (n, coords) = numbers[^1];
					n = n * 10 + (c - '0');
					coords.Add(coord);
					numbers[^1] = (n, coords);
				}
				else
				{
					numbers.Add((c - '0', new HashSet<Vector2> { coord }));
				}
			}
			else
			{
				symbols[coord] = c;
			}
		}
		return (numbers, symbols);
	}
}
