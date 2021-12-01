using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;

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
	}
}
