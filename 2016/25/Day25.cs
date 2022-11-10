using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2016;

[Day(25)]
public class Day25
{
	public static Assembler<Func<long, IEnumerable<long>>> AssemBunny { get; } = new(Day12.AssemBunny)
	{
		ArgNames = new[] { "a" },
		Header = "long b = 0, c = 0, d = 0;",
		["out"] = (t, ins) => $"yield return {t.ArgumentToExpression(ins, 0)};",
	};

	[Part(1)]
	public object Part1(string input)
	{
		var func = AssemBunny.Compile(input);
		for (long a = 0; true; a++)
		{
			if (func(a).Take(20).Chunk(2).All(c => c[0] == 0 && c[1] == 1))
			{
				return a;
			}
		}
	}
}
