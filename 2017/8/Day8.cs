using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(8)]
	public class Day8
	{
		private enum Cond
		{
			GreaterThan,
			LessThan,
			EqualTo,
			NotEqualTo,
			GreaterThanOrEqualTo,
			LessThanOrEqualTo,
		}

		private static Cond GetCond(string cond) => cond switch
		{
			"<" => Cond.LessThan,
			">" => Cond.GreaterThan,
			"<=" => Cond.LessThanOrEqualTo,
			">=" => Cond.GreaterThanOrEqualTo,
			"==" => Cond.EqualTo,
			"!=" => Cond.NotEqualTo,
			_ => throw new ArgumentException($"Unknown condition: {cond}")
		};

		private record Instruction(string reg, bool inc, int value, string condReg, Cond cond, int condValue);

		private static bool ShouldDo(Instruction i, int condValue) => i.cond switch
		{
			Cond.GreaterThan => condValue > i.condValue,
			Cond.LessThan => condValue < i.condValue,
			Cond.EqualTo => condValue == i.condValue,
			Cond.NotEqualTo => condValue != i.condValue,
			Cond.GreaterThanOrEqualTo => condValue >= i.condValue,
			Cond.LessThanOrEqualTo => condValue <= i.condValue,
			_ => throw new ArgumentException($"Unknown condition: {i.cond}")
		};

		[Part(1)]
		public object Part1(string input)
		{
			var instructions = input.Lines().Select(static l => {
				string[] w = l.Words();
				return new Instruction(w[0], w[1] == "inc", int.Parse(w[2]), w[4], GetCond(w[5]), int.Parse(w[6]));
			}).ToArray();

			Dictionary<string, int> registers = new Dictionary<string, int>();
			foreach (var i in instructions)
			{
				if (ShouldDo(i, registers.TryGetValue(i.condReg, out var condValue) ? condValue : 0))
				{
					registers[i.reg] = (registers.TryGetValue(i.reg, out var value) ? value : 0) + (i.inc ? i.value : -i.value);
				}
			}

			return registers.Values.Max();
		}

		[Part(2)]
		public object Part2(string input)
		{
			var instructions = input.Lines().Select(static l => {
				string[] w = l.Words();
				return new Instruction(w[0], w[1] == "inc", int.Parse(w[2]), w[4], GetCond(w[5]), int.Parse(w[6]));
			}).ToArray();

			int max = Int32.MinValue;

			Dictionary<string, int> registers = new Dictionary<string, int>();
			foreach (var i in instructions)
			{
				if (ShouldDo(i, registers.TryGetValue(i.condReg, out var condValue) ? condValue : 0))
				{
					registers[i.reg] = (registers.TryGetValue(i.reg, out var value) ? value : 0) + (i.inc ? i.value : -i.value);
					if (registers[i.reg] > max)
					{
						max = registers[i.reg];
					}
				}
			}

			return max;
		}
	}
}
