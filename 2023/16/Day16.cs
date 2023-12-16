using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2023;

[Day(16)]
public class Day16
{
	[Part(1)]
	public object Part1(string input)
	{
		var map = input.ToMap(c => c switch
			{
				'-' => Tile.Horizontal,
				'|' => Tile.Vertical,
				'/' => Tile.SWNE,
				'\\' => Tile.NWSE,
				_ => Tile.Empty,
			})
			.Where(kvp => kvp.Value != Tile.Empty)
			.ToDictionary();
		var max = map.Keys.Aggregate((a, b) => a.MaxParts(b));
		return Energize(map, max, (new(-1, 0), Vector2.Right));
	}

	[Part(2)]
	public object Part2(string input)
	{
		var map = input.ToMap(c => c switch
			{
				'-' => Tile.Horizontal,
				'|' => Tile.Vertical,
				'/' => Tile.SWNE,
				'\\' => Tile.NWSE,
				_ => Tile.Empty,
			})
			.Where(kvp => kvp.Value != Tile.Empty)
			.ToDictionary();
		var max = map.Keys.Aggregate((a, b) => a.MaxParts(b));

		return Enumerable.Range(0, (int)max.X + 1)
			.SelectMany<int, (Vector2, Vector2)>(x => [(new(x, -1), Vector2.Down), (new(x, max.Y + 1), Vector2.Up)])
			.Concat(Enumerable.Range(0, (int)max.Y + 1)
				.SelectMany<int, (Vector2, Vector2)>(y => [(new(-1, y), Vector2.Right), (new(max.X + 1, y), Vector2.Left)]))
			.AsParallel()
			.Max(s => Energize(map, max, s));
	}

	private static long Energize(IReadOnlyDictionary<Vector2, Tile> map, Vector2 max, (Vector2, Vector2) start)
	{
		var visitedTiles = new HashSet<Vector2>();
		var visited = new HashSet<(Vector2, Vector2)>();

		var toVisit = new Queue<(Vector2 pos, Vector2 dir)>();
		toVisit.Enqueue(start);

		while (toVisit.TryDequeue(out var current))
		{
			if (!visited.Add(current)) { continue; }
			var nextPos = current.pos + current.dir;
			if (nextPos.X < 0 || nextPos.Y < 0 || nextPos.X > max.X || nextPos.Y > max.X) { continue; }
			visitedTiles.Add(nextPos);
			if (!map.TryGetValue(nextPos, out var tile))
			{
				toVisit.Enqueue((nextPos, current.dir));
				continue;
			}
			switch (tile)
			{
				case Tile.Horizontal when current.dir == Vector2.Left || current.dir == Vector2.Right:
					toVisit.Enqueue((nextPos, current.dir)); // Pass through
					break;
				case Tile.Horizontal when current.dir == Vector2.Up || current.dir == Vector2.Down:
					toVisit.Enqueue((nextPos, Vector2.Left)); // Split
					toVisit.Enqueue((nextPos, Vector2.Right)); // Split
					break;

				case Tile.Vertical when current.dir == Vector2.Up || current.dir == Vector2.Down:
					toVisit.Enqueue((nextPos, current.dir)); // Pass through
					break;
				case Tile.Vertical when current.dir == Vector2.Left || current.dir == Vector2.Right:
					toVisit.Enqueue((nextPos, Vector2.Up)); // Split
					toVisit.Enqueue((nextPos, Vector2.Down)); // Split
					break;

				// /
				case Tile.SWNE when current.dir == Vector2.Up:
					toVisit.Enqueue((nextPos, Vector2.Right));
					break;
				case Tile.SWNE when current.dir == Vector2.Left:
					toVisit.Enqueue((nextPos, Vector2.Down));
					break;
				case Tile.SWNE when current.dir == Vector2.Right:
					toVisit.Enqueue((nextPos, Vector2.Up));
					break;
				case Tile.SWNE when current.dir == Vector2.Down:
					toVisit.Enqueue((nextPos, Vector2.Left));
					break;

				// \
				case Tile.NWSE when current.dir == Vector2.Up:
					toVisit.Enqueue((nextPos, Vector2.Left));
					break;
				case Tile.NWSE when current.dir == Vector2.Left:
					toVisit.Enqueue((nextPos, Vector2.Up));
					break;
				case Tile.NWSE when current.dir == Vector2.Right:
					toVisit.Enqueue((nextPos, Vector2.Down));
					break;
				case Tile.NWSE when current.dir == Vector2.Down:
					toVisit.Enqueue((nextPos, Vector2.Right));
					break;
			}
		}

		return visitedTiles.Count;
	}

	private enum Tile
	{
		Empty,
		Horizontal,
		Vertical,
		SWNE,
		NWSE,
	}
}
