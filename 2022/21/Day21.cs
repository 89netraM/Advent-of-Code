using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using System.Linq.Expressions;
using System.Reflection;

namespace AoC.Year2022;

#pragma warning disable CS8509, CS8846

[Day(21)]
public class Day21
{
	[Part(1)]
	public object Part1(string input)
	{
		var monkeys = input.Lines()
			.Select(l => l.Split(": "))
			.ToDictionary(p => p[0], p => ParseExpr(p[1]));

		return monkeys["root"](monkeys);
	}

	private delegate long MonkeyShout(IReadOnlyDictionary<string, MonkeyShout> monkeys);

	private MethodInfo IReadOnlyDictionaryAccess = typeof(IReadOnlyDictionary<string, MonkeyShout>).GetMethod("get_Item");

	private MonkeyShout ParseExpr(string input)
	{
		var monkeys = Expression.Parameter(typeof(IReadOnlyDictionary<string, MonkeyShout>), "monkeys");

		if (long.TryParse(input, out var constant))
		{
			return Expression.Lambda<MonkeyShout>(Expression.Constant(constant, typeof(long)), monkeys)
				.Compile();
		}

		var parts = input.Words();
		Expression left = long.TryParse(parts[0], out var lConst) ?
			Expression.Constant(lConst, typeof(long)) :
			Expression.Invoke(
				Expression.Call(monkeys, IReadOnlyDictionaryAccess, Expression.Constant(parts[0], typeof(string))),
				monkeys);
		Expression right = long.TryParse(parts[2], out var rConst) ?
			Expression.Constant(rConst, typeof(long)) :
			Expression.Invoke(
				Expression.Call(monkeys, IReadOnlyDictionaryAccess, Expression.Constant(parts[2], typeof(string))),
				monkeys);
		Expression op = parts[1] switch
		{
			"+" => Expression.Add(left, right),
			"-" => Expression.Subtract(left, right),
			"*" => Expression.Multiply(left, right),
			"/" => Expression.Divide(left, right),
			_ => throw new Exception("Unknown operator"),
		};

		return Expression.Lambda<MonkeyShout>(op, monkeys).Compile();
	}

	[Part(2)]
	public object Part2(string input)
	{
		var monkeys = input.Lines()
			.Select(l => l.Split(": "))
			.ToDictionary(p => p[0], p => p[1].Words());

		var expr = ParseMonkey(monkeys, "root");
		return expr switch
		{
			Eq { left: Const c } => c.constant,
			Eq { right: Const c } => c.constant,
		};
	}

	private IMonkey ParseMonkey(IReadOnlyDictionary<string, string[]> monkeys, string monkey)
	{
		if (monkey == "humn")
		{
			return new Me();
		}
		var parts = monkeys[monkey];
		if (parts.Length == 1)
		{
			return new Const(long.Parse(parts[0]));
		}
		var left = ParseMonkey(monkeys, parts[0]);
		var right = ParseMonkey(monkeys, parts[2]);
		return parts[1] switch
		{
			_ when monkey == "root" => Eq.Make(left, right),
			"+" => Add.Make(left, right),
			"-" => Sub.Make(left, right),
			"*" => Mul.Make(left, right),
			"/" => Div.Make(left, right),
		};
	}

	private interface IMonkey { }
	private record Me() : IMonkey
	{
		public override string ToString() =>
			"x";
	}
	private record Const(long constant) : IMonkey
	{
		public override string ToString() =>
			constant.ToString();
	}
	private record Add(IMonkey left, IMonkey right) : IMonkey
	{
		public static IMonkey Make(IMonkey left, IMonkey right) =>
			(left, right) switch
			{
				(Const l, Const r) => new Const(l.constant + r.constant),
				(var l, var r) => new Add(l, r),
			};

		public override string ToString() =>
			$"({left} + {right})";
	}
	private record Sub(IMonkey left, IMonkey right) : IMonkey
	{
		public static IMonkey Make(IMonkey left, IMonkey right) =>
			(left, right) switch
			{
				(Const l, Const r) => new Const(l.constant - r.constant),
				(var l, var r) => new Sub(l, r),
			};

		public override string ToString() =>
			$"({left} - {right})";
	}
	private record Mul(IMonkey left, IMonkey right) : IMonkey
	{
		public static IMonkey Make(IMonkey left, IMonkey right) =>
			(left, right) switch
			{
				(Const l, Const r) => new Const(l.constant * r.constant),
				(var l, var r) => new Mul(l, r),
			};

		public override string ToString() =>
			$"({left} * {right})";
	}
	private record Div(IMonkey left, IMonkey right) : IMonkey
	{
		public static IMonkey Make(IMonkey left, IMonkey right) =>
			(left, right) switch
			{
				(Const l, Const r) => new Const(l.constant / r.constant),
				(var l, var r) => new Div(l, r),
			};

		public override string ToString() =>
			$"({left} / {right})";
	}
	private record Eq(IMonkey left, IMonkey right) : IMonkey
	{
		public static Eq Make(IMonkey left, IMonkey right) =>
			(left, right) switch
			{
				(Add l, Const r) => (l.left, l.right) switch
				{
					// x + 4 = 8 => x = 8 - 4
					(var e, Const c) => Eq.Make(e, Sub.Make(r, c)),
					// 4 + x = 8 => x = 8 - 4
					(Const c, var e) => Eq.Make(e, Sub.Make(r, c)),
				},
				(Sub l, Const r) => (l.left, l.right) switch
				{
					// x - 4 = 8 => x = 8 + 4
					(var e, Const c) => Eq.Make(e, Add.Make(r, c)),
					// 4 - x = 8 => 4 = 8 + x => x = 4 - 8
					(Const c, var e) => Eq.Make(e, Sub.Make(c, r)),
				},
				(Mul l, Const r) => (l.left, l.right) switch
				{
					// x * 4 = 8 => x = 8 / 4
					(var e, Const c) => Eq.Make(e, Div.Make(r, c)),
					// 4 * x = 8 => x = 8 / 4
					(Const c, var e) => Eq.Make(e, Div.Make(r, c)),
				},
				(Div l, Const r) => (l.left, l.right) switch
				{
					// x / 4 = 8 => x = 8 * 4
					(var e, Const c) => Eq.Make(e, Mul.Make(r, c)),
					// 4 / x = 8 => 4 = 8 * x => x = 4 / 8
					(Const c, var e) => Eq.Make(e, Div.Make(c, r)),
				},
				(Const l, Const r) => new Eq(l, r),
				(Me l, Const r) => new Eq(l, r),
				(Const l, var r) => Eq.Make(r, l),
			};

		public override string ToString() =>
			$"{left} = {right}";
	}
}
