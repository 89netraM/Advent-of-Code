using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(18)]
	public class Day18
	{
		[Part(1)]
		public object Part1(string input)
		{
			string[][] inst = input.Lines().Select(static l => l.Words()).ToArray();
			var regs = new Dictionary<string, long>();
			long last = 0;
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

				if (parts[0] == "snd")
				{
					last = x;
				}
				else if (parts[0] == "set")
				{
					regs[parts[1]] = y;
				}
				else if (parts[0] == "add")
				{
					regs[parts[1]] = x + y;
				}
				else if (parts[0] == "mul")
				{
					regs[parts[1]] = x * y;
				}
				else if (parts[0] == "mod")
				{
					regs[parts[1]] = MathM.Mod(x, y);
				}
				else if (parts[0] == "rcv")
				{
					if (x != 0)
					{
						return last;
					}
				}
				else if (parts[0] == "jgz")
				{
					if (x > 0)
					{
						pc += y;
						continue;
					}
				}
				pc++;
			}
			return null;
		}

		[Part(2)]
		public object Part2(string input)
		{
			string[][] inst = input.Lines().Select(static l => l.Words()).ToArray();
			var regsA = new Dictionary<string, long>
			{
				["p"] = 0,
			};
			long pcA = 0;
			Queue<long> outputA = new Queue<long>();

			var regsB = new Dictionary<string, long>
			{
				["p"] = 1,
			};
			long pcB = 0;
			Queue<long> outputB = new Queue<long>();

			long sendACount = 0;
			long sendBCount = 0;

			while (Execute(inst, regsA, ref pcA, outputB, outputA, ref sendACount) | Execute(inst, regsB, ref pcB, outputA, outputB, ref sendBCount));

			return sendBCount;
		}

		private static bool Execute(string[][] inst, Dictionary<string, long> regs, ref long pc, Queue<long> input, Queue<long> output, ref long sendCount)
		{
			var parts = inst[pc];
			long x = long.TryParse(parts[1], out x) ? x : regs.GetValueOrDefault(parts[1], 0);
			long y = 0;
			if (parts.Length > 2)
			{
				y = long.TryParse(parts[2], out y) ? y : regs.GetValueOrDefault(parts[2], 0);
			}

			if (parts[0] == "snd")
			{
				output.Enqueue(x);
				sendCount++;
			}
			else if (parts[0] == "set")
			{
				regs[parts[1]] = y;
			}
			else if (parts[0] == "add")
			{
				regs[parts[1]] = x + y;
			}
			else if (parts[0] == "mul")
			{
				regs[parts[1]] = x * y;
			}
			else if (parts[0] == "mod")
			{
				regs[parts[1]] = MathM.Mod(x, y);
			}
			else if (parts[0] == "rcv")
			{
				if (input.TryDequeue(out y))
				{
					regs[parts[1]] = y;
				}
				else
				{
					return false;
				}
			}
			else if (parts[0] == "jgz")
			{
				if (x > 0)
				{
					pc += y;
					return true;
				}
			}
			pc++;
			return true;
		}
	}
}
