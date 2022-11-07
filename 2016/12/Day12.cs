using System;
using System.Collections.Generic;
using AoC.Library;
using RegExtract;

namespace AoC.Year2016;

[Day(12)]
public class Day12
{
	[Part(1)]
	public object Part1(string input)
	{
		var regs = new Dictionary<string, long>
		{
			["a"] = 0,
			["b"] = 0,
			["c"] = 0,
			["d"] = 0,
		};
		var instructions = input.Lines();
		for (int i = 0; i < instructions.Length; i++)
		{
			var ins = instructions[i];
			if (ins.StartsWith("cpy"))
			{
				var (from, to) = ins.Extract<(string, string)>(@"cpy (.*?) (.*)");
				if (Int64.TryParse(from, out long v))
				{
					regs[to] = v;
				}
				else
				{
					regs[to] = regs[from];
				}
			}
			else if (ins.StartsWith("inc"))
			{
				var reg = ins.Extract<string>(@"inc (.*)");
				regs[reg]++;
			}
			else if (ins.StartsWith("dec"))
			{
				var reg = ins.Extract<string>(@"dec (.*)");
				regs[reg]--;
			}
			else if (ins.StartsWith("jnz"))
			{
				var (x, offset) = ins.Extract<(string, string)>(@"jnz (.*?) (.*)");
				if (Int64.TryParse(x, out long v) ? v != 0 : regs[x] != 0)
				{
					i += Int32.Parse(offset) - 1;
				}
			}
		}
		return regs["a"];
	}

	public static Assembler AssemBunny { get; }
	static Day12()
	{
		var t = Transpiler.SingleCharRegister('a');
		AssemBunny = new Assembler
		{
			{ "cpy", ins => t.Assign(ins, 1, 0) },
			{ "inc", ins => t.Inc(ins, 0) },
			{ "dec", ins => t.Dec(ins, 0) },
			{ "jnz", ins => t.If(t.Ne(ins, 0, "0"), t.JumpOffset(ins, 1)) },
		};
	}

	[Part(2)]
	public object Part2(string input)
	{
		var function = AssemBunny.Compile(input);
		var regs = new long[] { 0, 0, 1, 0 };
		function(regs);
		return regs[0];
	}
}