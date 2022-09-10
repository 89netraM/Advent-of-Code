using System;
using System.Linq;
using AoC.Library;

namespace AoC.Year2015
{
	[Day(23)]
	public class Day23
	{
		private delegate void Ins(ref int i, ref ulong a, ref ulong b);

		[Part(1)]
		public object Part1(string input) =>
			RunFrom(input, 0, 0);

		[Part(2)]
		public object Part2(string input) =>
			RunFrom(input, 1, 0);

		public ulong RunFrom(string input, ulong a, ulong b)
		{
			var ins = input.Lines()
				.Select(ParseIns)
				.ToArray();

			for (int i = 0; i < ins.Length;)
			{
				ins[i](ref i, ref a, ref b);
			}
			return b;
		}

		private Ins ParseIns(string line) =>
			line[0..3] switch
			{
				"hlf" => line[4] == 'a' ? (ref int i, ref ulong a, ref ulong b) => { i++; a /= 2; } : (ref int i, ref ulong a, ref ulong b) => { i++; b /= 2; },
				"tpl" => line[4] == 'a' ? (ref int i, ref ulong a, ref ulong b) => { i++; a *= 3; } : (ref int i, ref ulong a, ref ulong b) => { i++; b *= 3; },
				"inc" => line[4] == 'a' ? (ref int i, ref ulong a, ref ulong b) => { i++; a++; } : (ref int i, ref ulong a, ref ulong b) => { i++; b++; },
				"jmp" => MakeJmp(line),
				"jie" => MakeJie(line),
				"jio" => MakeJio(line),
				_ => throw new Exception("Impossible!"),
			};

		private Ins MakeJmp(string line)
		{
			int offset = Int32.Parse(line[4..]);
			return (ref int i, ref ulong a, ref ulong b) => { i += offset; };
		}

		private Ins MakeJie(string line)
		{
			int offset = Int32.Parse(line[7..]);
			if (line[4] == 'a')
			{
				return (ref int i, ref ulong a, ref ulong b) =>
				{
					if (a % 2 == 0)
					{
						i += offset;
					}
					else
					{
						i++;
					}
				};
			}
			else
			{
				return (ref int i, ref ulong a, ref ulong b) =>
				{
					if (b % 2 == 0)
					{
						i += offset;
					}
					else
					{
						i++;
					}
				};
			}
		}

		private Ins MakeJio(string line)
		{
			int offset = Int32.Parse(line[7..]);
			if (line[4] == 'a')
			{
				return (ref int i, ref ulong a, ref ulong b) =>
				{
					if (a == 1)
					{
						i += offset;
					}
					else
					{
						i++;
					}
				};
			}
			else
			{
				return (ref int i, ref ulong a, ref ulong b) =>
				{
					if (b == 1)
					{
						i += offset;
					}
					else
					{
						i++;
					}
				};
			}
		}
	}
}
