using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using FsCheck;

namespace AoC.Year2021
{
	[Day(24)]
	public partial class Day24
	{
		[Part(0)]
		public object InputTest(string _)
		{
			Gen<long> digit = Gen.Choose(1, 9).Select(static i => (long)i);
			Gen<IList<long>> number = digit.ListOf(14);

			Prop.ForAll(
				number.ToArbitrary(),
				l => InputUnprocessed(l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9], l[10], l[11], l[12], l[13]).z == Input(l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9], l[10], l[11], l[12], l[13]).z
			).VerboseCheckThrowOnFailure();
			return null;
		}

		[Part(1)]
		public object Part1(string _)
		{
			// a = n - 4
			// b = g - 8
			// c = d + 1
			// e = f - 6
			// h = i
			// j = m - 5
			// k = l - 2
			// a b c d e f g h i j k l m n
			// 5 1 9 8 3 9 9 9 9 4 7 9 9 9
			return 51983999947999L;
		}

		[Part(2)]
		public object Part2(string _)
		{
			// a = n - 4
			// b = g - 8
			// c = d + 1
			// e = f - 6
			// h = i
			// j = m - 5
			// k = l - 2
			// a b c d e f g h i j k l m n
			// 1 1 2 1 1 7 9 1 1 1 1 3 6 5
			return 11211791111365L;
		}
	}
}
