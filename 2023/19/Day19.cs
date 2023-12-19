using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;
using System.Collections.Immutable;

namespace AoC.Year2023;

[Day(19)]
public class Day19
{
	[Part(1)]
	public object Part1(string input)
	{
		var paragraphs = input.Paragraphs();
		var workflows = paragraphs[0].Lines()
			.Extract<(string name, string flow)>(@"(.*?)\{(.*?)\}")
			.ToDictionary(p => p.name, p => ParseWorkflow(p.flow));
		var parts = paragraphs[1].Lines()
			.Extract<Part>(@"\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}");
		var accepted = new List<Part>();
		foreach (var part in parts)
		{
			var flow = "in";
			while (true)
			{
				flow = workflows[flow].Process(part);
				if (flow == "R")
				{
					break;
				}
				if (flow == "A")
				{
					accepted.Add(part);
					break;
				}
			}
		}
		return accepted.Sum(p => p.Total);
	}

	[Part(2)]
	public object Part2(string input)
	{
		var workflows = input.Paragraphs()[0]
			.Lines()
			.Extract<(string name, string flow)>(@"(.*?)\{(.*?)\}")
			.ToImmutableDictionary(p => p.name, p => ParseWorkflow(p.flow));
		return Recurse(new(new(1, 1, 1, 1), new(4000, 4000, 4000, 4000)), "in");

		long Recurse(Range range, string current)
		{
			if (current == "R") { return 0; }
			if (current == "A") { return range.Count; }

			var count = 0L;
			foreach (var branch in workflows[current].Branches)
			{
				if (range is null) { break; }
				(range, var nextRange) = branch switch
				{
					LessThan.X(var value, _) => (range with { Min = range.Min with { X = value } }, range with { Max = range.Max with { X = value - 1 } }),
					LessThan.M(var value, _) => (range with { Min = range.Min with { M = value } }, range with { Max = range.Max with { M = value - 1 } }),
					LessThan.A(var value, _) => (range with { Min = range.Min with { A = value } }, range with { Max = range.Max with { A = value - 1 } }),
					LessThan.S(var value, _) => (range with { Min = range.Min with { S = value } }, range with { Max = range.Max with { S = value - 1 } }),
					GreaterThan.X(var value, _) => (range with { Max = range.Max with { X = value } }, range with { Min = range.Min with { X = value + 1 } }),
					GreaterThan.M(var value, _) => (range with { Max = range.Max with { M = value } }, range with { Min = range.Min with { M = value + 1 } }),
					GreaterThan.A(var value, _) => (range with { Max = range.Max with { A = value } }, range with { Min = range.Min with { A = value + 1 } }),
					GreaterThan.S(var value, _) => (range with { Max = range.Max with { S = value } }, range with { Min = range.Min with { S = value + 1 } }),
					_ => (null, range),
				};
				count += Recurse(nextRange, branch.Outcome);
			}
			return count;
		}
	}

	private static Workflow ParseWorkflow(string flow)
	{
		var branches = flow.Split(',')
			.Select<string, IBranch>(b => b.Split(':') switch
			{
				[string outcome] => new Unconditional(outcome),
				[string condition, string outcome] => (condition[0], condition[1], long.Parse(condition[2..])) switch
				{
					('x', '<', long value) => new LessThan.X(value, outcome),
					('m', '<', long value) => new LessThan.M(value, outcome),
					('a', '<', long value) => new LessThan.A(value, outcome),
					('s', '<', long value) => new LessThan.S(value, outcome),
					('x', '>', long value) => new GreaterThan.X(value, outcome),
					('m', '>', long value) => new GreaterThan.M(value, outcome),
					('a', '>', long value) => new GreaterThan.A(value, outcome),
					('s', '>', long value) => new GreaterThan.S(value, outcome),
					_ => throw new Exception(),
				},
				_ => throw new Exception(),
			})
			.ToArray();
		return new(branches);
	}

	private record Workflow(IBranch[] Branches)
	{
		public string Process(Part part)
		{
			foreach (var branch in Branches)
			{
				if (branch.Test(part))
				{
					return branch.Outcome;
				}
			}
			throw new Exception();
		}
	}

	private interface IBranch
	{
		bool Test(Part part);
		string Outcome { get; }
	}
	private abstract record LessThan(long Value, string Outcome) : IBranch
	{
		public abstract bool Test(Part part);

		public record X(long Value, string Outcome) : LessThan(Value, Outcome)
		{
			public override bool Test(Part part) => part.X < Value;
		}
		public record M(long Value, string Outcome) : LessThan(Value, Outcome)
		{
			public override bool Test(Part part) => part.M < Value;
		}
		public record A(long Value, string Outcome) : LessThan(Value, Outcome)
		{
			public override bool Test(Part part) => part.A < Value;
		}
		public record S(long Value, string Outcome) : LessThan(Value, Outcome)
		{
			public override bool Test(Part part) => part.S < Value;
		}
	}
	private abstract record GreaterThan(long Value, string Outcome) : IBranch
	{
		public abstract bool Test(Part part);

		public record X(long Value, string Outcome) : GreaterThan(Value, Outcome)
		{
			public override bool Test(Part part) => part.X > Value;
		}
		public record M(long Value, string Outcome) : GreaterThan(Value, Outcome)
		{
			public override bool Test(Part part) => part.M > Value;
		}
		public record A(long Value, string Outcome) : GreaterThan(Value, Outcome)
		{
			public override bool Test(Part part) => part.A > Value;
		}
		public record S(long Value, string Outcome) : GreaterThan(Value, Outcome)
		{
			public override bool Test(Part part) => part.S > Value;
		}
	}
	private record Unconditional(string Outcome) : IBranch
	{
		public bool Test(Part part) => true;
	}

	private record Part(long X, long M, long A, long S)
	{
		public long Total => X + M + A + S;
	}

	private record Range(Part Min, Part Max)
	{
		public long Count => (Max.X - Min.X + 1) * (Max.M - Min.M + 1) * (Max.A - Min.A + 1) * (Max.S - Min.S + 1);
	}
}
