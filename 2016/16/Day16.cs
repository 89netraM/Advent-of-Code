using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using System.Numerics;
using System.Text;

namespace AoC.Year2016;

[Day(16)]
public class Day16
{
	[Part(1)]
	public string Part1(string input) =>
		ChecksumOfFullDisk(input, 272);

	private string ChecksumOfFullDisk(string input, int targetLength)
	{
		int length = input.Length;
		var data = ParseBinary(input);

		while (length < targetLength)
		{
			var b = Reverse(data, length);
			b = Invert(b, length);
			(data, length) = Join(data, b, length);
		}

		data = (data >> (length - targetLength)) & Mask(targetLength);
		length = targetLength;

		while (length % 2 == 0)
		{
			(data, length) = Checksum(data, length);
		}

		return PrintBinary(data, length);
	}

	private BigInteger ParseBinary(string input)
	{
		BigInteger data = 0;
		foreach (var c in input)
		{
			data = data << 1;
			if (c == '1')
			{
				data |= 1;
			}
		}
		return data;
	}

	private BigInteger Reverse(BigInteger input, int length)
	{
		BigInteger data = 0;
		for (int i = 0; i < length; i++)
		{
			data = data << 1;
			if ((input & 1) == 1)
			{
				data |= 1;
			}
			input = input >> 1;
		}
		return data;
	}

	private BigInteger Invert(BigInteger input, int length) =>
		(~input) & Mask(length);

	private BigInteger Mask(int length)
	{
		BigInteger data = 0;
		for (int i = 0; i < length; i++)
		{
			data = data << 1;
			data |= 1;
		}
		return data;
	}

	private (BigInteger, int) Join(BigInteger a, BigInteger b, int length) =>
		(a << (length + 1) | b, length * 2 + 1);

	private (BigInteger, int) Checksum(BigInteger input, int length)
	{
		BigInteger data = 0;
		length /= 2;
		for (int i = 0; i < length; i++)
		{
			data = data << 1;
			var pair = input & 0b11;
			if (pair == 0b00 || pair == 0b11)
			{
				data |= 1;
			}
			input = input >> 2;
		}
		return (Reverse(data, length), length);
	}

	private string PrintBinary(BigInteger input, int length)
	{
		BigInteger data = Reverse(input, length);
		var sb = new StringBuilder();
		for (int i = 0; i < length; i++)
		{
			if ((data & 1) == 1)
			{
				sb.Append('1');
			}
			else
			{
				sb.Append('0');
			}
			data = data >> 1;
		}
		return sb.ToString();
	}

	[Part(2)]
	public object Part2(string input) =>
		ChecksumOfFullDiskBig(input, 35651584);

	public string ChecksumOfFullDiskBig(string input, int targetLength)
	{
		int reductions = CalculateReductions(targetLength);
		var sequence = MakeSequence(input).Take(targetLength);
		var checksum = Reduce(sequence, reductions);
		return String.Concat(checksum);
	}

	public int CalculateReductions(int length)
	{
		int reductions = 0;
		while ((length & 1) == 0)
		{
			length >>= 1;
			reductions++;
		}
		return reductions;
	}

	public IEnumerable<char> Reduce(IEnumerable<char> sequence, int reductions)
	{
		if (reductions == 0)
		{
			return sequence;
		}
		else
		{
			return Reduce(
				sequence
					.Chunk(2)
					.Select(ReducePair),
				reductions - 1);
		}

		static char ReducePair(char[] cs) =>
			cs switch
			{
				[char c] => c,
				['0', '0'] => '1',
				['0', '1'] => '0',
				['1', '0'] => '0',
				['1', '1'] => '1',
				_ => throw new Exception(),
			};
	}

	public IEnumerable<char> MakeSequence(string input)
	{
		var glue = MakeGlueSequence().GetEnumerator();
		while (true)
		{
			foreach (var c in Mirror(input, glue))
			{
				yield return c;
			}
		}
	}

	public IEnumerable<char> Mirror(string input, IEnumerator<char> glue)
	{
		foreach (var c in input)
		{
			yield return c;
		}
		glue.MoveNext();
		yield return glue.Current;
		for (int i = input.Length - 1; i >= 0; i--)
		{
			yield return input[i] == '1' ? '0' : '1';
		}
		glue.MoveNext();
		yield return glue.Current;
	}

	public IEnumerable<char> MakeGlueSequence()
	{
		var seen = new List<char>();
		while (true)
		{
			int length = seen.Count;
			seen.Add('0');
			yield return '0';

			for (int i = length - 1; i >= 0; i--)
			{
				var c = seen[i] == '1' ? '0' : '1';
				yield return c;
				seen.Add(c);
			}
		}
	}
}
