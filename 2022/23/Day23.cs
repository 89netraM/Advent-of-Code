using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2022;

[Day(23)]
public class Day23
{
	private readonly (Vector2, Vector2, Vector2)[] Moves = new[]
	{
		(new Vector2(-1, -1), Vector2.Up, new Vector2(1, -1)),
		(new Vector2(1, 1), Vector2.Down, new Vector2(-1, 1)),
		(new Vector2(-1, -1), Vector2.Left, new Vector2(-1, 1)),
		(new Vector2(1, 1), Vector2.Right, new Vector2(1, -1)),
	};

	[Part(1)]
	public object Part1(string input)
	{
		var map = input.ToMap().Where(kvp => kvp.Value == '#').Select(kvp => kvp.Key).ToHashSet();
		var map2 = new HashSet<Vector2>(map.Count);
		var elfDirection = new Dictionary<Vector2, Vector2>(map.Count);
		var proposed = new Dictionary<Vector2, long>(map.Count);

		for (int round = 0; round < 10; round++)
		{
			foreach (var elf in map)
			{
				if (elf.NeighborsMoore().Any(map.Contains))
				{
					for (int move = 0; move < Moves.Length; move++)
					{
						var (left, forward, right) = Moves[(round + move) % Moves.Length];
						if (!map.Contains(elf + left) && !map.Contains(elf + forward) && !map.Contains(elf + right))
						{
							proposed.Increase(elf + forward);
							elfDirection.Add(elf, forward);
							break;
						}
					}
				}
			}

			foreach (var elf in map)
			{
				if (elfDirection.TryGetValue(elf, out var dir) && proposed[elf + dir] == 1)
				{
					map2.Add(elf + dir);
				}
				else
				{
					map2.Add(elf);
				}
			}

			(map, map2) = (map2, map);
			map2.Clear();
			elfDirection.Clear();
			proposed.Clear();
		}

		var min = map.Aggregate((a, e) => a.MinParts(e));
		var max = map.Aggregate((a, e) => a.MaxParts(e));
		var area = (max.X - min.X + 1) * (max.Y - min.Y + 1);
		return area - map.Count;
	}

	[Part(2)]
	public object Part2(string input)
	{
		var map = input.ToMap().Where(kvp => kvp.Value == '#').Select(kvp => kvp.Key).ToHashSet();
		var map2 = new HashSet<Vector2>(map.Count);
		var elfDirection = new Dictionary<Vector2, Vector2>(map.Count);
		var proposed = new Dictionary<Vector2, long>(map.Count);

		for (int round = 0; true; round++)
		{
			foreach (var elf in map)
			{
				if (elf.NeighborsMoore().Any(map.Contains))
				{
					for (int move = 0; move < Moves.Length; move++)
					{
						var (left, forward, right) = Moves[(round + move) % Moves.Length];
						if (!map.Contains(elf + left) && !map.Contains(elf + forward) && !map.Contains(elf + right))
						{
							proposed.Increase(elf + forward);
							elfDirection.Add(elf, forward);
							break;
						}
					}
				}
			}

			if (proposed.Count == 0)
			{
				return round + 1;
			}

			foreach (var elf in map)
			{
				if (elfDirection.TryGetValue(elf, out var dir) && proposed[elf + dir] == 1)
				{
					map2.Add(elf + dir);
				}
				else
				{
					map2.Add(elf);
				}
			}

			(map, map2) = (map2, map);
			map2.Clear();
			elfDirection.Clear();
			proposed.Clear();
		}
	}
}
