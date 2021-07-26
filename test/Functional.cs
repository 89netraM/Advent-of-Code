using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Linq;
using System.Collections.Generic;
using static AoC.Library.Functional;

namespace AoC.Library.Test
{
	[TestClass]
	public class FunctionalTest
	{
		[TestMethod]
		public void Id()
		{
			List<int> list = Enumerable.Range(0, 10).OrderByDescending(Id<int>).ToList();
			Assert.IsTrue(list.Zip(list.Skip(1), static (a, b) => (a, b)).All(static p => p.a > p.b));
		}

		[TestMethod]
		public void Not()
		{
			static bool IsLength2(string s) => s.Length == 2;
			Assert.IsTrue(new[] { "a", "b", "c" }.All(Not<string>(IsLength2)));
		}
	}
}
