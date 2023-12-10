using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2023;

[Day(10)]
public class Day10
{
	[Part(1)]
	public object Part1(string input)
	{
		var (start, map) = Parse(input);

		var (loop, _) = FindLoop(map, start);

		return loop.Length / 2L;
	}

	[Part(2)]
	public object Part2(string input)
	{
		var (start, map) = Parse(input);

		var (path, startSymbol) = FindLoop(map, start);
		map[start] = new(startSymbol);
		var loop = path.ToHashSet();

		return CountInsides(map, loop);
	}

	private static (Vector2, Dictionary<Vector2, Pipe>) Parse(string input)
	{
		var start = Vector2.Zero;
		var map = input.ToMap()
			.Where(kvp => { if (kvp.Value == 'S') { start = kvp.Key; } return kvp.Value != '.'; })
			.ToDictionary(kvp => kvp.Key, MakePipe);
		return (start, map);
	}

	private static Pipe MakePipe(KeyValuePair<Vector2, char> kvp) =>
		kvp.Value switch
		{
			'|' => new('|', kvp.Key + Vector2.Up, kvp.Key + Vector2.Down),
			'-' => new('-', kvp.Key + Vector2.Left, kvp.Key + Vector2.Right),
			'L' => new('L', kvp.Key + Vector2.Up, kvp.Key + Vector2.Right),
			'J' => new('J', kvp.Key + Vector2.Up, kvp.Key + Vector2.Left),
			'7' => new('7', kvp.Key + Vector2.Down, kvp.Key + Vector2.Left),
			'F' => new('F', kvp.Key + Vector2.Down, kvp.Key + Vector2.Right),
			'S' => new('S'),
			_ => throw new Exception("No"),
		};

	record Pipe(char Symbol, params Vector2[] Neighbors);

	private static (Vector2[], char) FindLoop(IReadOnlyDictionary<Vector2, Pipe> map, Vector2 start)
	{
		if (FindLoopFrom(start + Vector2.Up, out var path))
		{
			return (path, path[^1].X == start.X - 1 ? 'J' : 'L');
		}
		if (FindLoopFrom(start + Vector2.Right, out path))
		{
			return (path, path[^1].Y == start.Y - 1 ? 'L' : 'F');
		}
		if (FindLoopFrom(start + Vector2.Left, out path))
		{
			return (path, path[^1].Y == start.Y - 1 ? 'J' : '7');
		}
		if (FindLoopFrom(start + Vector2.Down, out path))
		{
			return (path, path[^1].X == start.X - 1 ? '7' : 'F');
		}
		return (Array.Empty<Vector2>(), 'S');

		bool FindLoopFrom(Vector2 from, out Vector2[] path)
		{
			if (map.TryGetValue(from, out var upNeighbors) && upNeighbors.Neighbors.Contains(start))
			{
				var isLoop = BFS.Search(
					from,
					n => n == from ? map[n].Neighbors.Where(n => n != start) : map[n].Neighbors,
					n => n == start,
					out var pathSequence);
				if (isLoop)
				{
					path = pathSequence.Prepend(from).ToArray();
					return true;
				}
			}
			path = Array.Empty<Vector2>();
			return false;
		}
	}

	private static long CountInsides(IReadOnlyDictionary<Vector2, Pipe> map, IReadOnlySet<Vector2> loop)
	{
		var min = map.Keys.Aggregate((a, b) => a.MinParts(b));
		var max = map.Keys.Aggregate((a, b) => a.MaxParts(b));
		long count = 0;
		for (long y = min.Y; y <= max.Y; y++)
		{
			var inside = false;
			for (long x = min.X; x <= max.X; x++)
			{
				var pos = new Vector2(x, y);
				if (loop.Contains(pos))
				{
					var symbol = map[pos].Symbol;
					if (symbol == '|')
					{
						inside = !inside;
					}
					else if (symbol is 'L' or 'F')
					{
						for (x++; x <= max.X; x++)
						{
							var nextSymbol = map[new(x, y)].Symbol;
							if (nextSymbol is '7' or 'J')
							{
								if (!((symbol, nextSymbol) is ('L', 'J') or ('F', '7')))
								{
									inside = !inside;
								}
								break;
							}
						}
					}
				}
				else if (inside)
				{
					count++;
				}
			}
		}
		return count;
	}
}
