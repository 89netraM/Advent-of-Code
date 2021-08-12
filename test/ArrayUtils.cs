using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace AoC.Library.Test
{
	[TestClass]
	public class ArrayUtilsTest
	{
		[TestMethod]
		public void IndexOf_Existing()
		{
			var array = new[] { 1, 2, 3, 4, 5 };
			Assert.AreEqual(1, ArrayUtils.IndexOf(array, 2));
		}

		[TestMethod]
		public void IndexOf_Missing()
		{
			var array = new[] { 1, 2, 3, 4, 5 };
			Assert.AreEqual(-1, ArrayUtils.IndexOf(array, 0));
		}
	}
}
