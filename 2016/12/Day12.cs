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

	[Part(2)]
	public object Part2(string input)
	{
		long a = 0, b = 0, c = 1, d = 0;

		// cpy 1 a
		a = 1;

		// cpy 1 b
		b = 1;

		// cpy 26 d
		d = 26;

		// jnz c 2
		if (c != 0)
		{
			goto ins6;
		}

		// jnz 1 5
		goto ins10;

		// cpy 7 c
	ins6:
		c = 7;

		// inc d
	ins7:
		d++;

		// dec c
		c--;

		// jnz c -2
		if (c != 0)
		{
			goto ins7;
		}

		// cpy a c
	ins10:
		c = a;

		// inc a
	ins11:
		a++;

		// dec b
		b--;

		// jnz b -2
		if (b != 0)
		{
			goto ins11;
		}
		
		// cpy c b
		b = c;

		// dec d
		d--;

		// jnz d -6
		if (d != 0)
		{
			goto ins10;
		}
		
		// cpy 18 c
		c = 18;

		// cpy 11 d
	ins18:
		d = 11;

		// inc a
	ins19:
		a++;

		// dec d
		d--;

		// jnz d -2
		if (d != 0)
		{
			goto ins19;
		}
		
		// dec c
		c--;

		// jnz c -5
		if (c != 0)
		{
			goto ins18;
		}

		return a;
	}
}
