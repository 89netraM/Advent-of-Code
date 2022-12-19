using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using Google.OrTools.Sat;

namespace AoC.Year2022;

[Day(19)]
public class Day19
{
	[Part(1)]
	public object Part1(string input) =>
		input.Lines()
			.Select(ParseBlueprint)
			.Select((b, i) => CollectGeodes(b, 24) * (i + 1))
			.Sum();

	[Part(2)]
	public object Part2(string input) =>
		input.Lines()
			.Take(3)
			.Select(ParseBlueprint)
			.Select(b => CollectGeodes(b, 32))
			.Product();

	private Dictionary<string, Dictionary<string, long>> ParseBlueprint(string input) =>
		input.Words()[2..]
			.Let(ws => String.Join(' ', ws))
			.Split('.', StringSplitOptions.RemoveEmptyEntries)
			.ToDictionary(
				l => l.Words()[1],
				l => l.Words()
					.Window()
					.Where(p => p.Item1.All(Char.IsDigit))
					.ToDictionary(p => p.Item2, p => long.Parse(p.Item1)));

	private long CollectGeodes(Dictionary<string, Dictionary<string, long>> blueprint, int minutes)
	{
		var model = new CpModel();

		var buildBotDuring = new Dictionary<string, List<BoolVar>>();
		foreach (var mineral in blueprint.Keys)
		{
			buildBotDuring[mineral] = new();
			for (int i = 0; i <= minutes; i++)
				buildBotDuring[mineral].Add(model.NewBoolVar($"{mineral}_{{{i}}}"));
		}
		for (int i = 0; i < minutes; i++)
		{
			model.Add(LinearExpr.Sum(blueprint.Keys.Select(m => buildBotDuring[m][i])) <= 1);
		}

		var botsAfter = new Dictionary<string, List<LinearExpr>>();
		foreach (var mineral in blueprint.Keys)
		{
			var start = mineral == "ore" ? 1 : 0;
			botsAfter[mineral] = new();
			for (int i = 0; i <= minutes; i++)
				if (i == 0)
					botsAfter[mineral].Add(start + buildBotDuring[mineral][i]);
				else
					botsAfter[mineral].Add(botsAfter[mineral][^1] + buildBotDuring[mineral][i]);
		}

		var countAfter = new Dictionary<string, List<LinearExpr>>();
		foreach (var mineral in blueprint.Keys)
		{
			var start = mineral == "ore" ? 1 : 0;
			countAfter[mineral] = new();
			for (int i = 0; i <= minutes; i++)
			{
				var cost = LinearExpr.Sum(blueprint
					.Where(kvp => kvp.Value.ContainsKey(mineral))
					.Select(kvp => buildBotDuring[kvp.Key][i] * kvp.Value[mineral]));
				if (i <= 1)
					countAfter[mineral].Add(start - cost);
				else
					countAfter[mineral].Add(countAfter[mineral][^1] + botsAfter[mineral][i - 2] - cost);
				model.Add(countAfter[mineral][^1] >= 0);
			}
		}

		model.Maximize(countAfter["geode"][^1]);
		var solver = new CpSolver();
		solver.Solve(model);
		return (long)solver.ObjectiveValue;
	}
}
