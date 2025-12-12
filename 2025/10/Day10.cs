using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using AoC.Library;
using Microsoft.Z3;
using RegExtract;
using static AoC.Library.Functional;

namespace AoC.Year2025;

[Day(10)]
public class Day10
{
	[Part(1)]
	public object Part1(string input) => Parse(input).AsParallel().Sum(Solve);

	[Part(2)]
	public object Part2(string input)
	{
		return null;
	}

	private static IEnumerable<Manual> Parse(string input) =>
		input
			.Lines()
			.Extract<(string, string, string)>(@"^\[(.*)\] (.*?) {(.*)}$")
			.Select(t => new Manual(
				[.. t.Item1.Select(c => c is '#')],
				[.. t.Item2.Words().Select(b => b[1..^1].Split(',').Select(int.Parse).ToArray())],
				[.. t.Item3.Split(',').Select(int.Parse)]
			));

	private static long Solve(Manual manual)
	{
		using var ctx = new Context();
		var optimizer = ctx.MkOptimize();
		var buttons = manual.Buttons.Select((_, i) => ctx.MkIntConst($"button_{i}")).ToArray();
		for (var i = 0; i < manual.Lights.Length; i++)
		{
			var equation = manual
				.Buttons.Select((b, i) => (b, i))
				.Where(p => p.b.Contains(i))
				.Select(p => -buttons[p.i] + 2 * ctx.MkIntConst($"button_diff_{i}"))
				.Aggregate((a, b) => a + b);
			optimizer.Assert(ctx.MkEq(equation, ctx.MkInt(manual.Lights[i] ? 1 : 0)));
		}
		optimizer.MkMinimize(buttons.Aggregate<ArithExpr>((a, b) => a + b));
		return optimizer.Check() is Status.SATISFIABLE
			? ((IntNum)optimizer.Model.Eval(optimizer.Objectives[0])).Int64
			: throw new Exception("Not satisfiable.");
	}

	private record Manual(bool[] Lights, int[][] Buttons, int[] Joltages);
}
