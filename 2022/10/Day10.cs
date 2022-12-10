using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2022;

[Day(10)]
public class Day10
{
	[Part(1)]
	public object Part1(string input)
	{
		var important = new long[] { 20, 60, 100, 140, 180, 220 };
		long x = 1;
		long signal = 0;
		long c = 1;
		foreach (var inst in input.Lines())
		{
			if (inst == "noop")
			{
				c++;
				if (important.Contains(c))
				{
					signal += c * x;
				}
			}
			else
			{
				c++;
				if (important.Contains(c))
				{
					signal += c * x;
				}
				x += long.Parse(inst.Substring(4));
				c++;
				if (important.Contains(c))
				{
					signal += c * x;
				}
			}
		}
		return signal;
	}

	[Part(2)]
	public object Part2(string input)
	{
		long x = 1;
		var screen = new HashSet<long>();
		long c = 0;
		foreach (var inst in input.Lines())
		{
			if (inst == "noop")
			{
				if (MathM.Mod(c, 40) == x - 1 || MathM.Mod(c, 40) == x || MathM.Mod(c, 40) == x + 1)
				{
					screen.Add(c);
				}
				c++;
				if (MathM.Mod(c, 40) == x - 1 || MathM.Mod(c, 40) == x || MathM.Mod(c, 40) == x + 1)
				{
					screen.Add(c);
				}
			}
			else
			{
				if (MathM.Mod(c, 40) == x - 1 || MathM.Mod(c, 40) == x || MathM.Mod(c, 40) == x + 1)
				{
					screen.Add(c);
				}
				c++;
				if (MathM.Mod(c, 40) == x - 1 || MathM.Mod(c, 40) == x || MathM.Mod(c, 40) == x + 1)
				{
					screen.Add(c);
				}
				x += long.Parse(inst.Substring(4));
				c++;
				if (MathM.Mod(c, 40) == x - 1 || MathM.Mod(c, 40) == x || MathM.Mod(c, 40) == x + 1)
				{
					screen.Add(c);
				}
			}
		}
		for (long row = 0; row < 6; row++)
		{
			for (long col = 0; col < 40; col++)
			{
				if (screen.Contains(row * 40 + col))
				{
					Console.Write('#');
				}
				else
				{
					Console.Write('.');
				}
			}
			Console.WriteLine();
		}
		return null;
	}
}
