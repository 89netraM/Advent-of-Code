using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2023;

[Day(2)]
public class Day2
{
	[Part(1)]
	public object Part1(string input)
	{
		return input.Lines()
			.Extract<(long id, string games)>(@"Game (\d+): (.*)")
			.Select(p => (p.id, games: GetGames(p.games)))
			.Where(p => p.games.All(g => g.r <= 12 && g.g <= 13 && g.b <= 14))
			.Sum(p => p.id);
	}

	private static List<(long r, long g, long b)> GetGames(string games) =>
		games.Split(";")
			.Select(g => g.Split(",").Select(c => c.Trim().Split(" ")).ToDictionary(c => c[1], c => long.Parse(c[0])))
			.Select(g => (g.GetValueOrDefault("red", 0), g.GetValueOrDefault("green", 0), g.GetValueOrDefault("blue", 0)))
			.ToList();

	[Part(2)]
	public object Part2(string input)
	{
		return input.Lines()
			.Extract<(long id, string games)>(@"Game (\d+): (.*)")
			.Select(p => (p.id, games: GetGames(p.games)))
			.Select(p => (r: p.games.Max(g => g.r), g: p.games.Max(g => g.g), b: p.games.Max(g => g.b)))
			.Sum(p => p.r * p.g * p.b);
	}
}
