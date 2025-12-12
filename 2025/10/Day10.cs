using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using Microsoft.Z3;
using RegExtract;

namespace AoC.Year2025;

[Day(10)]
public class Day10
{
	[Part(1)]
	public object Part1(string input) =>
		Parse(input)
			.AsParallel()
			.Sum(manual =>
			{
				using var ctx = new Context();
				var optimizer = ctx.MkOptimize();
				var buttons = manual
					.Buttons.Select(
						(_, i) =>
						{
							var button = ctx.MkIntConst($"button_{i}");
							optimizer.Assert(button >= 0);
							return button;
						}
					)
					.ToArray();
				var lights = manual
					.Lights.Select(
						(_, i) =>
						{
							var light = ctx.MkIntConst($"light_{i}");
							optimizer.Assert(light >= 0);
							return light;
						}
					)
					.ToArray();
				for (var i = 0; i < manual.Lights.Length; i++)
				{
					var equation =
						2 * lights[i]
						- manual
							.Buttons.Select((b, i) => (b, i))
							.Where(p => p.b.Contains(i))
							.Select(p => buttons[p.i])
							.Aggregate<ArithExpr>((a, b) => a + b);
					optimizer.Assert(ctx.MkEq(equation, ctx.MkInt(manual.Lights[i] ? 1 : 0)));
				}
				var buttonCount = buttons.Aggregate<ArithExpr>((a, b) => a + b);
				optimizer.MkMinimize(buttonCount);
				return optimizer.Check() is Status.SATISFIABLE
					? ((IntNum)optimizer.Model.Eval(buttonCount)).Int64
					: throw new Exception("Not satisfiable.");
			});

	[Part(2)]
	public object Part2(string input) =>
		Parse(input)
			.AsParallel()
			.Sum(manual =>
			{
				using var ctx = new Context();
				var optimizer = ctx.MkOptimize();
				var buttons = manual
					.Buttons.Select(
						(_, i) =>
						{
							var button = ctx.MkIntConst($"button_{i}");
							optimizer.Assert(button >= 0);
							return button;
						}
					)
					.ToArray();
				for (var i = 0; i < manual.Lights.Length; i++)
				{
					var equation = manual
						.Buttons.Select((b, i) => (b, i))
						.Where(p => p.b.Contains(i))
						.Select(p => buttons[p.i])
						.Aggregate<ArithExpr>((a, b) => a + b);
					optimizer.Assert(ctx.MkEq(equation, ctx.MkInt(manual.Joltages[i])));
				}
				var buttonCount = buttons.Aggregate<ArithExpr>((a, b) => a + b);
				optimizer.MkMinimize(buttonCount);
				return optimizer.Check() is Status.SATISFIABLE
					? ((IntNum)optimizer.Model.Eval(buttonCount)).Int64
					: throw new Exception("Not satisfiable.");
			});

	private static IEnumerable<Manual> Parse(string input) =>
		input
			.Lines()
			.Extract<(string, string, string)>(@"^\[(.*)\] (.*?) {(.*)}$")
			.Select(t => new Manual(
				[.. t.Item1.Select(c => c is '#')],
				[.. t.Item2.Words().Select(b => b[1..^1].Split(',').Select(int.Parse).ToArray())],
				[.. t.Item3.Split(',').Select(int.Parse)]
			));

	private record Manual(bool[] Lights, int[][] Buttons, int[] Joltages);
}
