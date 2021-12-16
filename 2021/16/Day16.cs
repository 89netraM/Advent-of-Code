using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using OneOf;

namespace AoC.Year2021
{
	[Day(16)]
	public class Day16
	{
		[Part(1)]
		public object Part1(string input)
		{
			static long Visit(BITS bits) =>
				bits.Version + bits.Value.Match(
					_ => 0L,
					children => children.Sum(Visit)
				);
			return Visit(BITS.Parse(input));
		}

		[Part(2)]
		public object Part2(string input)
		{
			static long Evaluate(BITS node) =>
				node.Value.Match(
					v => v,
					children => node.Type switch
					{
						0 => children.Sum(Evaluate),
						1 => children.Product(Evaluate),
						2 => children.Min(Evaluate),
						3 => children.Max(Evaluate),
						5 => Evaluate(children[0]) > Evaluate(children[1]) ? 1L : 0L,
						6 => Evaluate(children[0]) < Evaluate(children[1]) ? 1L : 0L,
						7 => Evaluate(children[0]) == Evaluate(children[1]) ? 1L : 0L,
						_ => throw new InvalidOperationException(),
					}
				);
			return Evaluate(BITS.Parse(input));
		}
	}

	public class BITS
	{
		/// <summary>
		/// Turns a hexadecimal string into a sequence of bits. Each bit is represented by a long 1 or 0.
		/// </summary>
		public static IEnumerable<long> ParseBitStream(string input) =>
			input.SelectMany(static c =>
			{
				long b = ((long)c & 0b1111L) + (c > '9' ? 9L : 0L);
				return new long[] { (b & 0b1000L) >> 3, (b & 0b0100L) >> 2, (b & 0b0010L) >> 1, b & 0b0001L };
			});

		/// <summary>
		/// Turns a sequence of bits into a tree of BITS nodes.
		/// </summary>
		/// <param name="input"></param>
		/// <param name="versionWidth">The number of bits that represent the version number.</param>
		/// <param name="typeWidth">The number of bits that represent the type number</param>
		/// <param name="literalType">The type number of packets that contain literal values, as opposed to a sequence of child packets.</param>
		/// <param name="literalChunkWidth">The number of bits in a chunk of literal values.</param>
		/// <param name="childBitCountWidth">The number of bits that represent the bit-count of children.</param>
		/// <param name="childCountWidth">The number of bits that represent number of children.</param>
		/// <inheritdoc cref="InternalParse"/>
		public static BITS Parse(string input, int versionWidth = 3, int typeWidth = 3, long literalType = 4L, int literalChunkWidth = 5, int childBitCountWidth = 15, int childCountWidth = 11)
		{
			long literalChunkMask = 0L;
			for (int i = 0; i < literalChunkWidth - 1; i++)
			{
				literalChunkMask = literalChunkMask << 1 | 0b1L;
			}

			using var bits = ParseBitStream(input).GetEnumerator();
			return InternalParse(bits, versionWidth, typeWidth, literalType, literalChunkWidth, literalChunkMask, childBitCountWidth, childCountWidth).packet;
		}

		/// <inheritdoc cref="Next"/>
		private static (BITS packet, long length) InternalParse(IEnumerator<long> bits, int versionWidth, int typeWidth, long literalType, int literalChunkWidth, long literalChunkMask, int childBitCountWidth, int childCountWidth)
		{
			long version = Next(bits, versionWidth);
			long type = Next(bits, typeWidth);
			long length = versionWidth + typeWidth;
			if (type == literalType)
			{
				long continueMask = 0b1L << (literalChunkWidth - 1);
				long part = continueMask;
				long value = 0L;
				while ((part & continueMask) != 0L)
				{
					part = Next(bits, literalChunkWidth);
					length += literalChunkWidth;
					value = (value << (literalChunkWidth - 1)) | (part & literalChunkMask);
				}
				return (new BITS(version, type, value), length);
			}
			else
			{
				long lengthType = Next(bits);
				length += 1;
				List<BITS> children = new List<BITS>();
				if (lengthType == 0L)
				{
					long bitCount = Next(bits, childBitCountWidth);
					length += childBitCountWidth;
					long start = length;
					while (length - start < bitCount)
					{
						var (child, childLength) = InternalParse(bits, versionWidth, typeWidth, literalType, literalChunkWidth, literalChunkMask, childBitCountWidth, childCountWidth);
						length += childLength;
						children.Add(child);
					}
				}
				else
				{
					long packetCount = Next(bits, childCountWidth);
					length += childCountWidth;
					for (long i = 0; i < packetCount; i++)
					{
						var (child, childLength) = InternalParse(bits, versionWidth, typeWidth, literalType, literalChunkWidth, literalChunkMask, childBitCountWidth, childCountWidth);
						length += childLength;
						children.Add(child);
					}
				}
				return (new BITS(version, type, children), length);
			}
		}

		/// <summary>
		/// Moves and returns the next bit of the enumerator, or throws an exception if the enumerator is exhausted.
		/// </summary>
		/// <param name="bits">The enumerator to move.</param>
		/// <exception cref="InvalidOperationException">The enumerator is exhausted.</exception>
		public static long Next(IEnumerator<long> bits) =>
			bits.MoveNext() ? bits.Current : throw new InvalidOperationException("Unexpected end of stream");
		/// <summary>
		/// Moves the enumerator and returns the value represented by the consumed bits, or throws an exception if the enumerator is exhausted.
		/// </summary>
		/// <param name="count">The number of bits to consume.</param>
		/// <inheritdoc cref="Next"/>
		public static long Next(IEnumerator<long> bits, int count)
		{
			long result = 0L;
			for (int i = 0; i < count; i++)
			{
				result = (result << 1) | Next(bits);
			}
			return result;
		}

		/// <summary>
		/// The version of the packet.
		/// </summary>
		public long Version { get; }
		/// <summary>
		/// The type of the packet.
		/// </summary>
		public long Type { get; }
		/// <summary>
		/// Either the literal value of the packet, or a list of the packets children.
		/// </summary>
		public OneOf<long, IReadOnlyList<BITS>> Value { get; }

		public BITS(long version, long type, OneOf<long, IReadOnlyList<BITS>> value) =>
			(Version, Type, Value) = (version, type, value);
	}
}
