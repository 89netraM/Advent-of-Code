using System;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using static AoC.Library.Functional;

namespace AoC.Library.Test
{
	[TestClass]
	public class IEnumerableUtilsTest
	{
		private readonly long[] sequence = new long[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

		[TestMethod]
		public void Window_PairwiseSum()
		{
			Assert.AreEqual(81L, sequence.Window().Select(static p => p.Item1 + p.Item2).Sum());
		}

		[TestMethod]
		public void Window3_TriplewiseSum()
		{
			Assert.AreEqual(108L, sequence.Window3().Select(static p => p.Item1 + p.Item2 + p.Item3).Sum());
		}

		[TestMethod]
		public void Window4_FourwiseSum()
		{
			Assert.AreEqual(126L, sequence.Window4().Select(static p => p.Item1 + p.Item2 + p.Item3 + p.Item4).Sum());
		}

		[TestMethod]
		public void Product_long()
		{
			Assert.AreEqual(362880L, sequence.Skip(1).Product());
		}

		[TestMethod]
		public void Product_transformer()
		{
			Assert.AreEqual(3628800L, sequence.Skip(1).Product(static l => l + 1L));
		}

		[TestMethod]
		public void AdjacentGroupBy_UniqueList()
		{
			Assert.AreEqual(sequence.Length, sequence.AdjacentGroupBy(Id).Count());
		}

		[TestMethod]
		public void AdjacentGroupBy_NoAdjacentDuplicates()
		{
			var sequence = new[] { 0, 1, 2, 0, 1, 2 };
			Assert.AreEqual(sequence.Length, sequence.AdjacentGroupBy(Id).Count());
		}

		[TestMethod]
		public void AdjacentGroupBy_PairOfDuplicates()
		{
			var sequence = new[] { 0, 0, 1, 1, 2, 2 };
			Assert.AreEqual(sequence.Length / 2, sequence.AdjacentGroupBy(Id).Count());
		}

		[TestMethod]
		public void AdjacentGroupBy_EnumeratingGroup()
		{
			var sequence = new[] { 0, 0, 0, 0, 0 };
			var onlyGroup = sequence.AdjacentGroupBy(Id).Single();
			Assert.IsTrue(sequence.SequenceEqual(onlyGroup));
		}

		[TestMethod]
		public void Enumerate_ShouldEnumerate()
		{
			var array = sequence.Enumerate().ToArray();
			for (int i = 0; i < array.Length; i++)
			{
				Assert.AreEqual(i, array[i].Key);
			}
		}

		[TestMethod]
		public void SelectWhere_Struct()
		{
			Assert.IsTrue(sequence.SelectWhere(i => i % 2 == 0 ? (long?)(i / 2) : null).SequenceEqual(new long[] { 0, 1, 2, 3, 4 }));
		}

		[TestMethod]
		public void SelectWhere_Class()
		{
			Assert.IsTrue(sequence.SelectWhere(i => i % 2 == 0 ? new LongBox(i / 2) : null)
				.SequenceEqual(new[] { new LongBox(0), new LongBox(1), new LongBox(2), new LongBox(3), new LongBox(4) }));
		}

		private record LongBox(long Long);

		[TestMethod]
		public void Transpose_Square()
		{
			var arr = new[]
			{
				new[] { 1, 2 },
				new[] { 3, 4 },
			};
			var transArr = new[]
			{
				new[] { 1, 3 },
				new[] { 2, 4 },
			};
			Assert.IsTrue(arr.Transpose().SequenceEqual(transArr, new SequenceEqualityComparer<int>()));
		}

		[TestMethod]
		public void Transpose_Wide()
		{
			var arr = new[]
			{
				new[] { 1, 2, 3 },
				new[] { 4, 5, 6 },
			};
			var transArr = new[]
			{
				new[] { 1, 4 },
				new[] { 2, 5 },
				new[] { 3, 6 },
			};
			Assert.IsTrue(arr.Transpose().SequenceEqual(transArr, new SequenceEqualityComparer<int>()));
		}

		[TestMethod]
		public void Transpose_Tall()
		{
			var arr = new[]
			{
				new[] { 1, 2 },
				new[] { 3, 4 },
				new[] { 5, 6 },
			};
			var transArr = new[]
			{
				new[] { 1, 3, 5 },
				new[] { 2, 4, 6 },
			};
			Assert.IsTrue(arr.Transpose().SequenceEqual(transArr, new SequenceEqualityComparer<int>()));
		}

		[TestMethod]
		public void Transpose_Jagged()
		{
			var arr = new[]
			{
				new[] { 1, 2, 3 },
				new[] { 4, 5 },
				new[] { 6, 7, 8 },
			};
			var transArr = new[]
			{
				new[] { 1, 4, 6 },
				new[] { 2, 5, 7 },
				new[] { 3, 8 },
			};
			Assert.IsTrue(arr.Transpose().SequenceEqual(transArr, new SequenceEqualityComparer<int>()));
		}

		[TestMethod]
		public void ToHexString_Array()
		{
			var arr = new byte[] { 0xff, 0x5a, 0x01, 0x44, };
			Assert.AreEqual("ff5a0144", arr.ToHexString());
		}

		[TestMethod]
		public void ToHexString_Enumerable()
		{
			var enumerable = Enumerable.Range(7, 3)
				.Concat(Enumerable.Range(0xf3, 3))
				.Select(i => (byte)i);
			Assert.AreEqual("070809f3f4f5", enumerable.ToHexString());
		}

		[TestMethod]
		public void ToHexString_Span()
		{
			Span<byte> span = stackalloc byte[] { 0xff, 0x5a, 0x01, 0x44, };
			Assert.AreEqual("ff5a0144", span.ToHexString());
		}
	}
}
