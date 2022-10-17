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
	}
}
