using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using AoC.Library;
using RegExtract;

namespace AoC.Year2016;

[Day(23)]
public class Day23
{
	[Part(1)]
	public object Part1(string input)
	{
		var regs = new Dictionary<string, long>
		{
			["a"] = 7,
			["b"] = 0,
			["c"] = 0,
			["d"] = 0,
		};
		RunProgram(input, regs);
		return regs["a"];
	}

	[Part(2)]
	public object Part2(string input)
	{
		var regs = new Dictionary<string, long>
		{
			["a"] = 12,
			["b"] = 0,
			["c"] = 0,
			["d"] = 0,
		};
		RunProgram(input, regs);
		return regs["a"];
	}

	private void RunProgram(string input, IDictionary<string, long> regs)
	{
		var instructions = input.Lines();
		for (int i = 0; i < instructions.Length; i++)
		{
			var ins = instructions[i];
			if (ins.StartsWith("cpy"))
			{
				var (from, to) = ins.Extract<(string, string)>(@"cpy (\S+) (\S+)");
				if (to.All(Char.IsAsciiLetter))
				{
					if (Int64.TryParse(from, out long v))
					{
						regs[to] = v;
					}
					else
					{
						regs[to] = regs[from];
					}
				}
			}
			else if (ins.StartsWith("inc"))
			{
				var reg = ins.Extract<string>(@"inc (\S+)");
				regs[reg]++;
			}
			else if (ins.StartsWith("dec"))
			{
				var reg = ins.Extract<string>(@"dec (\S+)");
				regs[reg]--;
			}
			else if (ins.StartsWith("jnz"))
			{
				var (x, arg) = ins.Extract<(string, string)>(@"jnz (\S+) (\S+)");
				var offset = Int32.TryParse(arg, out int o) ? o : (int)regs[arg];
				if (Int64.TryParse(x, out long v) ? v != 0 : regs[x] != 0)
				{
					i += offset - 1;
				}
			}
			else if (ins.StartsWith("mul"))
			{
				var (aArg, bArg, to) = ins.Extract<(string, string, string)>(@"mul (\S+) (\S+) (\S+)");
				var a = Int64.TryParse(aArg, out long aT) ? aT : regs[aArg];
				var b = Int64.TryParse(bArg, out long bT) ? bT : regs[bArg];
				regs[to] += a * b;
			}
			else if (ins.StartsWith("tgl"))
			{
				var arg = ins.Extract<string>(@"tgl (\S+)");
				var offset = Int64.TryParse(arg, out long v) ? v : regs[arg];
				var index = i + offset;

				if (0 <= index && index < instructions.Length)
				{
					var otherIns = instructions[index];
					if (otherIns.StartsWith("tgl"))
					{
						instructions[index] = "inc" + otherIns.Substring(3);
					}
					else if (otherIns.StartsWith("jnz"))
					{
						instructions[index] = "cpy" + otherIns.Substring(3);
					}
					else if (Regex.IsMatch(otherIns, @"... \S+ \S+"))
					{
						instructions[index] = "jnz" + otherIns.Substring(3);
					}
					else if (otherIns.StartsWith("inc"))
					{
						instructions[index] = "dec" + otherIns.Substring(3);
					}
					else if (Regex.IsMatch(otherIns, @"... \S+"))
					{
						instructions[index] = "inc" + otherIns.Substring(3);
					}
				}
			}
		}
	}
}
