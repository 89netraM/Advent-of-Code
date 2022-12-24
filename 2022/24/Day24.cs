using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using System.Collections.Immutable;

namespace AoC.Year2022;

#pragma warning disable CS8524, CS8509

[Day(24)]
public class Day24
{
	private Vector2 Start;
	private Vector2 Goal;
	private Vector2 Min;
	private Vector2 Max;
	private List<ImmutableDictionary<Vector2, Tile>> Maps;

	private void Setup(string input)
	{
		var inputMap = input.ToMap()
			.Where(kvp => kvp.Value != '#');

		Start = inputMap.MinBy(kvp => kvp.Key.Y).Key;
		Goal = inputMap.MaxBy(kvp => kvp.Key.Y).Key;

		Maps = new List<ImmutableDictionary<Vector2, Tile>>();
		Maps.Add(inputMap.Where(kvp => kvp.Value != '.').ToImmutableDictionary(kvp => kvp.Key, kvp => ToTile(kvp.Value)));
		Min = Start + Vector2.Down;
		Max = Goal + Vector2.Up;
	}

	[Part(1)]
	public object Part1(string input)
	{
		Setup(input);
		BFS.Search(
			(pos: Start, map: 0),
			MakeMoves,
			p => p.pos == Goal,
			out var path);
		return path.Count();
	}

	[Part(2)]
	public object Part2(string input)
	{
		Setup(input);
		BFS.Search(
			(pos: Start, map: 0),
			MakeMoves,
			p => p.pos == Goal,
			out var toGoal);
		BFS.Search(
			(pos: Goal, map: toGoal.Last().map),
			MakeMoves,
			p => p.pos == Start,
			out var toStart);
		BFS.Search(
			(pos: Start, map: toStart.Last().map),
			MakeMoves,
			p => p.pos == Goal,
			out var backToGoal);
		return toGoal.Count() + toStart.Count() + backToGoal.Count();
	}

	private IEnumerable<(Vector2, int)> MakeMoves((Vector2, int) state)
	{
		var (pos, map) = state;
		var nextMap = GetMap(map + 1);
		foreach (var nextPos in pos.NeighborsVonNeumann().Append(pos))
		{
			if ((IsInBounds(nextPos) || nextPos == Start || nextPos == Goal) && !nextMap.ContainsKey(nextPos))
			{
				yield return (nextPos, map + 1);
			}
		}
	}

	private ImmutableDictionary<Vector2, Tile> GetMap(int mapIndex)
	{
		if (mapIndex < Maps.Count)
		{
			return Maps[mapIndex];
		}

		for (int i = Maps.Count - 1; i <= mapIndex; i++)
		{
			var map = Maps[i];
			var mapBuilder = ImmutableDictionary.CreateBuilder<Vector2, Tile>();
			foreach (var (pos, tiles) in map)
			{
				foreach (var tile in Enum.GetValues<Tile>())
				{
					if (tiles.HasFlag(tile))
					{
						var nextPos = pos + Direction(tile);
						if (!IsInBounds(nextPos))
						{
							nextPos = tile switch
							{
								Tile.Up    => new Vector2(pos.X, Max.Y),
								Tile.Right => new Vector2(Min.X, pos.Y),
								Tile.Left  => new Vector2(Max.X, pos.Y),
								Tile.Down  => new Vector2(pos.X, Min.Y),
							};
						}
						mapBuilder.AddOrUpdate(nextPos, tile, t => t | tile);
					}
				}
			}
			Maps.Add(mapBuilder.ToImmutable());
		}
		return Maps[mapIndex];
	}

	private bool IsInBounds(Vector2 pos) =>
		Min.X <= pos.X && pos.X <= Max.X &&
			Min.Y <= pos.Y && pos.Y <= Max.Y;

	private Vector2 Direction(Tile tile) => tile switch
	{
		Tile.Up    => Vector2.Up,
		Tile.Right => Vector2.Right,
		Tile.Left  => Vector2.Left,
		Tile.Down  => Vector2.Down,
	};

	private Tile ToTile(char c) => c switch
	{
		'^' => Tile.Up,
		'>' => Tile.Right,
		'<' => Tile.Left,
		'v' => Tile.Down,
	};

	[Flags]
	private enum Tile
	{
		Up    = 0b0001,
		Right = 0b0010,
		Left  = 0b0100,
		Down  = 0b1000,
	}
}
