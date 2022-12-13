using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using AoC.Library;

namespace AoC.Year2022;

[Day(12)]
public class Day12
{
	[Part(1)]
	public object Part1(string input)
	{
		var map = input.ToMap();
		var start = map.Single(kvp => kvp.Value == 'S').Key;
		map[start] = 'a';
		var end = map.Single(kvp => kvp.Value == 'E').Key;
		map[end] = 'z';

		BFS.Search(
			start,
			f => f.NeighborsVonNeumann().Where(n => map.ContainsKey(n) && (map[n] - map[f]) <= 1),
			f => f == end,
			out var path);
		return path.Count();
	}

	[Part(2)]
	public object Part2(string input)
	{
		var map = input.ToMap();
		var start = map.Single(kvp => kvp.Value == 'S').Key;
		map[start] = 'a';
		var end = map.Single(kvp => kvp.Value == 'E').Key;
		map[end] = 'z';

		long shortest = long.MaxValue;
		foreach (var a in map.Where(kvp => kvp.Value == 'a').Select(kvp => kvp.Key))
		{
			if (BFS.Search(
				a,
				f => f.NeighborsVonNeumann().Where(n => map.ContainsKey(n) && (map[n] - map[f]) <= 1),
				f => f == end,
				out var path))
			{
				shortest = Math.Min(path.Count(), shortest);
			}
		}
		return shortest;
	}

	[Part(3)]
	public object Part3Visualization(string input)
	{
		var map = input.ToMap();
		var start = map.Single(kvp => kvp.Value == 'S').Key;
		map[start] = 'a';
		var end = map.Single(kvp => kvp.Value == 'E').Key;
		map[end] = 'z';

		BFS.Search(
			start,
			f => f.NeighborsVonNeumann().Where(n => map.ContainsKey(n) && (map[n] - map[f]) <= 1),
			f => f == end,
			out var path);

		Console.CursorVisible = false;
		Console.OutputEncoding = Encoding.UTF8;
		Console.Write("\x1B[2J");
		map[start] = 'S';
		map[end] = 'E';
		var prev = start;
		Print(map);
		foreach (var step in path)
		{
			map[prev] = (step - prev) switch
			{
				(0, -1) => '↑',
				(-1, 0) => '←',
				(1, 0) => '→',
				(0, 1) => '↓',
				_ => throw new Exception(),
			};
			Print(map, prev);
			prev = step;
		}
		Console.CursorVisible = true;

		return null;
	}

	private void Print(IReadOnlyDictionary<Vector2, char> map, Vector2? curr = null)
	{
		Console.Write("\x1B[H\x1B[90m\x1B[2m");
		var pos = Vector2.Zero;
		for (; map.ContainsKey(pos); pos += Vector2.Down)
		{
			for (; map.ContainsKey(pos); pos += Vector2.Right)
			{
				if (pos == curr)
				{
					Console.Write("\x1B[22m\x1B[38;5;11m");
				}
				else if (Char.IsAsciiLetterUpper(map[pos]))
				{
					Console.Write("\x1B[0m");
				}
				else if (!Char.IsAsciiLetter(map[pos]))
				{
					Console.Write("\x1B[22m\x1B[38;5;214m");
				}
				Console.Write(map[pos]);
				if (pos == curr || Char.IsAsciiLetterUpper(map[pos]) || !Char.IsAsciiLetter(map[pos]))
				{
					Console.Write("\x1B[90m\x1B[2m");
				}
			}
			pos = new(0, pos.Y);
			Console.WriteLine();
		}
		Console.Write("\x1B[0m");
	}
}
