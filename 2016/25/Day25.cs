using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2016;

[Day(25)]
public class Day25
{
	[Part(1)]
	public object Part1(string input)
	{
		for (long a = 0; true; a++)
		{
			if (Program(a).Take(20).Chunk(2).All(c => c[0] == 0 && c[1] == 1))
			{
				return a;
			}
		}
	}

	private IEnumerable<long> Program(long a)
	{
		long b = 0, c = 0, d = 0;
		// cpy a d
		d = a;
		// cpy 4 c
		// cpy 643 b
		// inc d
		// dec b
		// jnz b -2
		// dec c
		// jnz c -5
		b = 0;
		c = 0;
		d += 4 * 643;
	line9:
		// cpy d a
		a = d;
	line10:
		// jnz 0 0
		// cpy a b
		b = a;
		// cpy 0 a
		a = 0;
	line13:
		// cpy 2 c
		c = 2;
	line14:
		// jnz b 2
		if (b != 0)
			goto line16;
		// jnz 1 6
		goto line21;
	line16:
		// dec b
		b--;
		// dec c
		c--;
		// jnz c -4
		if (c != 0)
			goto line14;
		// inc a
		a++;
		// jnz 1 -7
		goto line13;
	line21:
		// cpy 2 b
		b = 2;
	line22:
		// jnz c 2
		if (c != 0)
			goto line24;
		// jnz 1 4
		goto line27;
	line24:
		// dec b
		b--;
		// dec c
		c--;
		// jnz 1 -4
		goto line22;
	line27:
		// jnz 0 0
		// out b
		yield return b;
		// jnz a -19
		if (a != 0)
			goto line10;
		// jnz 1 -21
		goto line9;
	}
}
