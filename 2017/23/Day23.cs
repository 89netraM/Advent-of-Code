using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(23)]
	public class Day23
	{
		[Part(1)]
		public object Part1(string input)
		{
			string[][] inst = input.Lines().Select(static l => l.Words()).ToArray();
			var regs = new Dictionary<string, long>();
			long mulCount = 0;
			long pc = 0;

			while (0 <= pc && pc < inst.Length)
			{
				var parts = inst[pc];
				long x = long.TryParse(parts[1], out x) ? x : regs.GetValueOrDefault(parts[1], 0);
				long y = 0;
				if (parts.Length > 2)
				{
					y = long.TryParse(parts[2], out y) ? y : regs.GetValueOrDefault(parts[2], 0);
				}

				if (parts[0] == "set")
				{
					regs[parts[1]] = y;
				}
				else if (parts[0] == "sub")
				{
					regs[parts[1]] = x - y;
				}
				else if (parts[0] == "mul")
				{
					regs[parts[1]] = x * y;
					mulCount++;
				}
				else if (parts[0] == "jnz")
				{
					if (x != 0)
					{
						pc += y;
						continue;
					}
				}
				pc++;
			}

			return mulCount;
		}

		[Part(2)]
		public object Part2(string input)
		{
			long a = 1;
			long b = 0;
			long c = 0;
			long d = 0;

			long f = 0;
			long g = 0;
			long h = 0;

			b = 93;
			c = b;
			if (a != 0)
			{
				b *= 100;
				b -= -100000;
				c = b;
				c -= -17000;
			}
			while (true)
			{
				f = 1;
				d = 2;
				do
				{
					if (b % d == 0 && (b / d) <= b)
					{
						f = 0;
					}
					d -= -1;
					g = d;
					g -= b;
				} while (g != 0);
				if (f == 0)
				{
					h -= -1;
				}
				g = b;
				g -= c;
				if (g == 0)
				{
					break;
				}
				b -= -17;
			}
			return h;
		}
	}
}
