// --- Day 21: Step Counter ---
// You manage to catch the airship right as it's dropping someone else off on their all-expenses-paid trip to Desert Island! It even helpfully drops you off near the gardener and his massive farm.
// "You got the sand flowing again! Great work! Now we just need to wait until we have enough sand to filter the water for Snow Island and we'll have snow again in no time."
// While you wait, one of the Elves that works with the gardener heard how good you are at solving problems and would like your help. He needs to get his steps in for the day, and so he'd like to know which garden plots he can reach with exactly his remaining 64 steps.
// He gives you an up-to-date map (your puzzle input) of his starting position (S), garden plots (.), and rocks (#). For example:
// ...........
// .....###.#.
// .###.##..#.
// ..#.#...#..
// ....#.#....
// .##..S####.
// .##..#...#.
// .......##..
// .##.#.####.
// .##..##.##.
// ...........
// The Elf starts at the starting position (S) which also counts as a garden plot. Then, he can take one step north, south, east, or west, but only onto tiles that are garden plots. This would allow him to reach any of the tiles marked O:
// ...........
// .....###.#.
// .###.##..#.
// ..#.#...#..
// ....#O#....
// .##.OS####.
// .##..#...#.
// .......##..
// .##.#.####.
// .##..##.##.
// ...........
// Then, he takes a second step. Since at this point he could be at either tile marked O, his second step would allow him to reach any garden plot that is one step north, south, east, or west of any tile that he could have reached after the first step:
// ...........
// .....###.#.
// .###.##..#.
// ..#.#O..#..
// ....#.#....
// .##O.O####.
// .##.O#...#.
// .......##..
// .##.#.####.
// .##..##.##.
// ...........
// After two steps, he could be at any of the tiles marked O above, including the starting position (either by going north-then-south or by going west-then-east).
// A single third step leads to even more possibilities:
// ...........
// .....###.#.
// .###.##..#.
// ..#.#.O.#..
// ...O#O#....
// .##.OS####.
// .##O.#...#.
// ....O..##..
// .##.#.####.
// .##..##.##.
// ...........
// He will continue like this until his steps for the day have been exhausted. After a total of 6 steps, he could reach any of the garden plots marked O:
// ...........
// .....###.#.
// .###.##.O#.
// .O#O#O.O#..
// O.O.#.#.O..
// .##O.O####.
// .##.O#O..#.
// .O.O.O.##..
// .##.#.####.
// .##O.##.##.
// ...........
// In this example, if the Elf's goal was to get exactly 6 more steps today, he could use them to reach any of 16 garden plots.
// However, the Elf actually needs to get 64 steps today, and the map he's handed you is much larger than the example map.
// Starting from the garden plot marked S on your map, how many garden plots could the Elf reach in exactly 64 steps?
// To begin, get your puzzle input.
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using AoC.Library;
using static AoC.Library.Functional;
using RegExtract;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Perfolizer.Horology;
using BenchmarkDotNet.Attributes;
using System.ComponentModel;
using FastSerialization;
using Microsoft.CodeAnalysis;

namespace AoC.Year2023;

[Day(21)]
public class Day21
{
	[Part(1)]
	public object Part1(string input)
	{
		var map = input.ToMap();
		var poss = new HashSet<Vector2>();
		BFS.Search(
			(pos: map.Single(kvp => kvp.Value == 'S').Key, steps: 0L),
			p => p.pos.NeighborsVonNeumann()
				.Where(pos => map.TryGetValue(pos, out var t) && t is '.' or 'S')
				.Select(pos => (pos, p.steps + 1L)),
			p => p.steps > 64,
			out _,
			p =>
			{
				if (p.steps == 64)
				{
					poss.Add(p.pos);
				}
			});
		return poss.Count;
	}

	[Part(3)]
	public object Part3(string input)
	{
		var map = input.ToMap();
		var start = map.Single(kvp => kvp.Value == 'S').Key;
		var max = map.Keys.Aggregate((a, b) => a.MaxParts(b));
		var half = max.X / 2;
		var width = max.X + 1;

		var halfMapValues = new List<long>();

		var prev = new HashSet<Vector2> { start };
		var next = new HashSet<Vector2>();
		for (long step = 1; step <= width * 2 + half; step++)
		{
			foreach (var pos in prev)
			{
				foreach (var n in pos.NeighborsVonNeumann())
				{
					if (GetTile(n) is '.' or 'S')
					{
						next.Add(n);
					}
				}
			}

			if (step >= half && (step - half) % width == 0)
			{
				halfMapValues.Add(next.Count);
			}
			(prev, next) = (next, prev);
			next.Clear();
		}

		halfMapValues.Dump();

		return FindPolynomial(halfMapValues.Select((y, i) => ((Int128)(half + width * i), (Int128)y)), 26501365L);

		char GetTile(Vector2 pos) =>
			map[new(MathM.Mod(pos.X, max.X + 1), MathM.Mod(pos.Y, max.Y + 1))];
	}

	private static Int128 FindPolynomial(IEnumerable<(Int128, Int128)> points, Int128 x)
	{
		var (a, b, c, denominator) = FindCoefficients(points);

		var xSq = x * x;
		var axSq = a * xSq;
		var bx = b * x;
		var ansNumerator = axSq + bx + c;
		return ansNumerator / denominator;
	}

	private static (Int128, Int128, Int128, Int128) FindCoefficients(IEnumerable<(Int128 x, Int128 y)> points)
	{
		if (points.ToArray() is not [(var x1, var y1), (var x2, var y2), (var x3, var y3)])
		{
			throw new Exception("No");
		}

		var denominator = -(x1 - x2) * (x1 - x3) * (x2 - x3);

		var a = (y1 * (x2 - x3) + y2 * (x3 - x1) + y3 * (x1 - x2)) * -1;
		var b = y1 * (x2 + x3) * (x2 - x3) + y2 * (x3 + x1) * (x3 - x1) + y3 * (x1 + x2) * (x1 - x2);
		var c = (y1 * x2 * x3 * (x2 - x3) + y2 * x3 * x1 * (x3 - x1) + y3 * x1 * x2 * (x1 - x2)) * -1;
		return (a, b, c, denominator);
	}

	[Part(2)]
	public object Part2(string input)
	{
		var map = input.ToMap();
		var max = map.Keys.Aggregate((a, b) => a.MaxParts(b));
		var fromToMap = new Dictionary<Vector2, Dictionary<Vector2, long>>();
		var totalSteps = 26501365L;

		var start = map.Single(kvp => kvp.Value == 'S').Key;
		var northEast = new Vector2(0, 0);
		var north = new Vector2(start.X, 0);
		var northWest = new Vector2(max.X, 0);
		var east = new Vector2(0, start.Y);
		var west = new Vector2(max.X, start.Y);
		var southEast = new Vector2(0, max.Y);
		var south = new Vector2(start.X, max.Y);
		var southWest = new Vector2(max.X, max.Y);
		foreach (var point in new[] { start, northEast, north, northWest, east, west, southEast, south, southWest })
		{
			fromToMap[point] = new Dictionary<Vector2, long>();
			DistanceFrom(fromToMap[point], point);
		}

		var count = CountReachableFrom(fromToMap[start], totalSteps);
		count += CountGoEast(totalSteps - fromToMap[start][east] - 1);
		count += CountGoNorthEast(totalSteps - fromToMap[start][northEast] - 2);
		count += CountGoSouthEast(totalSteps - fromToMap[start][southEast] - 2);
		count += CountGoWest(totalSteps - fromToMap[start][west] - 1);
		count += CountGoNorthWest(totalSteps - fromToMap[start][northWest] - 2);
		count += CountGoSouthWest(totalSteps - fromToMap[start][southWest] - 2);
		count += CountGoNorth(totalSteps - fromToMap[start][north] - 1);
		count += CountGoSouth(totalSteps - fromToMap[start][south] - 1);
		return count;

		long CountGoEast(long steps) => CountFromTo(steps, west, east).Sum();

		long CountGoNorthEast(long steps)
		{
			var (aHead, aEnd) = CountGoEastBottom(steps - fromToMap[southWest][southEast] - 1);
			var (bHead, bEnd) = CountGoNorthLeft(steps - fromToMap[southWest][northWest] - 1);
			return CountFromToDiagonal(steps, southWest, northEast, aHead + bHead, aEnd + bEnd);
		}

		(long, long) CountGoEastBottom(long steps) => CountFromTo(steps, southWest, southEast);

		(long, long) CountGoNorthLeft(long steps) => CountFromTo(steps, southWest, northWest);

		long CountGoSouthEast(long steps)
		{
			var (aHead, aEnd) = CountGoEastTop(steps - fromToMap[northWest][northEast] - 1);
			var (bHead, bEnd) = CountGoSouthLeft(steps - fromToMap[northWest][southWest] - 1);
			return CountFromToDiagonal(steps, northWest, southEast, aHead + bHead, aEnd + bEnd);
		}

		(long, long) CountGoEastTop(long steps) => CountFromTo(steps, northWest, northEast);

		(long, long) CountGoSouthLeft(long steps) => CountFromTo(steps, northWest, southWest);

		long CountGoWest(long steps) => CountFromTo(steps, east, west).Sum();

		long CountGoNorthWest(long steps)
		{
			var (aHead, aEnd) = CountGoWestBottom(steps - fromToMap[southEast][southWest] - 1);
			var (bHead, bEnd) = CountGoNorthRight(steps - fromToMap[southEast][northEast] - 1);
			return CountFromToDiagonal(steps, southEast, northWest, aHead + bHead, aEnd + bEnd);
		}

		(long, long) CountGoWestBottom(long steps) => CountFromTo(steps, southEast, southWest);

		(long, long) CountGoNorthRight(long steps) => CountFromTo(steps, southEast, northEast);

		long CountGoSouthWest(long steps)
		{
			var (aHead, aEnd) = CountGoWestTop(steps - fromToMap[northEast][northWest] - 1);
			var (bHead, bEnd) = CountGoSouthRight(steps - fromToMap[northEast][southEast] - 1);
			return CountFromToDiagonal(steps, northEast, southWest, aHead + bHead, aEnd + bEnd);
		}

		(long, long) CountGoWestTop(long steps) => CountFromTo(steps, northEast, northWest);

		(long, long) CountGoSouthRight(long steps) => CountFromTo(steps, northEast, southEast);

		long CountGoNorth(long steps) => CountFromTo(steps, south, north).Sum();

		long CountGoSouth(long steps) => CountFromTo(steps, north, south).Sum();

		(long, long) CountFromTo(long steps, Vector2 from, Vector2 to)
		{
			var length = fromToMap[from][to] + 1;
			var evenCount = CountReachableFrom(fromToMap[from], steps);
			var oddCount = CountReachableFrom(fromToMap[from], steps + 1);
			var maps = steps / length / 2;
			return (evenCount * (maps + steps % 2) + oddCount * (maps + (steps + 1) % 2), CountReachableFrom(fromToMap[from], steps % length));
		}

		long CountFromToDiagonal(long steps, Vector2 from, Vector2 to, long tail, long head)
		{
			var length = fromToMap[from][to] + 2;
			var count = CountReachableFrom(fromToMap[from], steps);
			var maps = steps / length;
			return count * maps + CountReachableFrom(fromToMap[from], steps % length);
		}

		void DistanceFrom(IDictionary<Vector2, long> fromToMap, Vector2 start)
		{
			fromToMap[start] = 0L;
			BFS.Search(
				start,
				p => p.NeighborsVonNeumann()
					.Where(pos => map.TryGetValue(pos, out var t) && t is '.' or 'S')
					.Select(pos => (pos, 1L)),
				_ => false,
				out _,
				(p, d) => fromToMap[p] = d);
		}

		static long CountReachableFrom(IReadOnlyDictionary<Vector2, long> fromToMap, long steps) =>
			fromToMap.Values.LongCount(dist => dist <= steps && long.IsOddInteger(dist) == long.IsOddInteger(steps));
	}
}

public static class TupleExtensions
{
	public static long Sum(this (long, long) pair) =>
		pair.Item1 + pair.Item2;
}
