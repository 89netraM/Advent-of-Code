using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2021
{
	[Day(3)]
	public class Day3
	{
		[Part(1)]
		public object Part1(string input)
		{
			int length = input.Lines().First().Length;
			long[] ones = new long[length];
			long[] zeroes = new long[length];
			foreach (var line in input.Lines())
			{
				for (int i = 0; i < line.Length; i++)
				{
					if (line[i] == '1')
					{
						ones[i]++;
					}
					else
					{
						zeroes[i]++;
					}
				}
			}
			long gamma = ones.Zip(zeroes, (o, z) => o > z ? 1 : 0).Select((c, i) => c * (long)Math.Pow(2, length - 1 - i)).Sum();
			long epsilon = zeroes.Zip(ones, (z, o) => z > o ? 1 : 0).Select((c, i) => c * (long)Math.Pow(2, length - 1 - i)).Sum();
			return gamma * epsilon;
		}

		[Part(2)]
		public object Part2(string input)
		{
			int length = input.Lines().First().Length;
			List<string> lines = input.Lines().ToList();
			for (int i = 0; lines.Count > 1 && i < length; i++)
			{
				long ones = 0;
				long zeroes = 0;
				foreach (var line in lines)
				{
					if (line[i] == '1')
					{
						ones++;
					}
					else
					{
						zeroes++;
					}
				}

				for (int j = 0; j < lines.Count; j++)
				{
					if ((ones >= zeroes && lines[j][i] == '0') || (ones < zeroes && lines[j][i] == '1'))
					{
						lines.RemoveAt(j);
						j--;
					}
				}
			}
			long oxygen = lines.Single().Select((c, i) => (c - '0') * (long)Math.Pow(2, length - 1 - i)).Sum();

			lines = input.Lines().ToList();
			for (int i = 0; lines.Count > 1 && i < length; i++)
			{
				long ones = 0;
				long zeroes = 0;
				foreach (var line in lines)
				{
					if (line[i] == '1')
					{
						ones++;
					}
					else
					{
						zeroes++;
					}
				}

				for (int j = 0; j < lines.Count; j++)
				{
					if ((ones < zeroes && lines[j][i] == '0') || (ones >= zeroes && lines[j][i] == '1'))
					{
						lines.RemoveAt(j);
						j--;
					}
				}
			}
			long oc2 = lines.Single().Select((c, i) => (c - '0') * (long)Math.Pow(2, length - 1 - i)).Sum();
			return oxygen * oc2;
		}
	}
}
