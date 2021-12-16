using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;

namespace AoC.Year2021
{
	[Day(16)]
	public class Day16
	{
		[Part(1)]
		public object Part1(string input)
		{
			IReadOnlyList<bool> bits = input.SelectMany(static c => Convert.ToString(Convert.ToByte(c.ToString(), 16), 2).PadLeft(4, '0')).Select(static c => c == '1').ToArray();

			long versionSum = 0L;
			for (int i = 0; bits.Skip(i).Any(Id);)
			{
				var version = GetBits(bits, ref i, 3);
				versionSum += version;
				var type = GetBits(bits, ref i, 3);
				if (type == 4L)
				{
					long part = 0b10000L;
					while ((part & 0b10000L) != 0L)
					{
						part = GetBits(bits, ref i, 5);
					}
				}
				else
				{
					var lengthType = GetBits(bits, ref i, 1);
					i += lengthType == 0L ? 15 : 11;
				}
			}
			return versionSum;
		}

		[Part(2)]
		public object Part2(string input)
		{
			IReadOnlyList<bool> bits = input.SelectMany(static c => Convert.ToString(Convert.ToByte(c.ToString(), 16), 2).PadLeft(4, '0')).Select(static c => c == '1').ToArray();
			int i = 0;
			return EvaluatePackate(bits, ref i);
		}

		private long EvaluatePackate(IReadOnlyList<bool> bits, ref int i)
		{
			var version = GetBits(bits, ref i, 3);
			var type = GetBits(bits, ref i, 3);
			if (type == 4L)
			{
				long part = 0b10000L;
				long value = 0L;
				while ((part & 0b10000L) != 0L)
				{
					part = GetBits(bits, ref i, 5);
					value = (value << 4) | (part & 0b1111L);
				}
				return value;
			}
			else
			{
				var lengthType = GetBits(bits, ref i, 1);
				List<long> subValues;
				if (lengthType == 0L)
				{
					long bitCount = GetBits(bits, ref i, 15);
					int start = i;
					subValues = new List<long>();
					while (i - start < bitCount)
					{
						subValues.Add(EvaluatePackate(bits, ref i));
					}
				}
				else
				{
					int packetCount = (int)GetBits(bits, ref i, 11);
					subValues = new List<long>(packetCount);
					for (int j = 0; j < packetCount; j++)
					{
						subValues.Add(EvaluatePackate(bits, ref i));
					}
				}
				return type switch
				{
					0L => subValues.Sum(),
					1L => subValues.Aggregate(1L, static (a, b) => a * b),
					2L => subValues.Min(),
					3L => subValues.Max(),
					5L => subValues[0] > subValues[1] ? 1L : 0L,
					6L => subValues[0] < subValues[1] ? 1L : 0L,
					7L => subValues[0] == subValues[1] ? 1L : 0L,
					_ => throw new Exception($"Invalid type {type}"),
				};
			}
			throw new Exception("Malformatted packet");
		}

		private long GetBits(IReadOnlyList<bool> bits, ref int i, int count)
		{
			long result = 0L;
			for (int j = 0; j < count; j++)
			{
				result = (result << 1) | (bits[i++] ? 1L : 0L);
			}
			return result;
		}
	}
}
