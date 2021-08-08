using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace AoC.Library.Test
{
	[TestClass]
	public class StringUtilsTest
	{
		[TestMethod]
		public void Lines_LineFeed()
		{
			const string input = "a\nb\nc";
			Assert.IsTrue(new[] { "a", "b", "c" }.SequenceEqual(input.Lines()));
		}

		[TestMethod]
		public void Lines_CarriageReturnLineFeed()
		{
			const string input = "a\r\nb\r\nc";
			Assert.IsTrue(new[] { "a", "b", "c" }.SequenceEqual(input.Lines()));
		}

		[TestMethod]
		public void Words_SingleSpace()
		{
			const string input = "a b c";
			Assert.IsTrue(new[] { "a", "b", "c" }.SequenceEqual(input.Words()));
		}

		[TestMethod]
		public void Lines_SingleTab()
		{
			const string input = "a\tb\tc";
			Assert.IsTrue(new[] { "a", "b", "c" }.SequenceEqual(input.Words()));
		}

		[TestMethod]
		public void Words_SingleLineFeed()
		{
			const string input = "a\nb\nc";
			Assert.IsTrue(new[] { "a", "b", "c" }.SequenceEqual(input.Words()));
		}

		[TestMethod]
		public void Lines_SingleCarriageReturnLineFeed()
		{
			const string input = "a\r\nb\r\nc";
			Assert.IsTrue(new[] { "a", "b", "c" }.SequenceEqual(input.Words()));
		}

		[TestMethod]
		public void Words_MultipleSpace()
		{
			const string input = "a    b    c";
			Assert.IsTrue(new[] { "a", "b", "c" }.SequenceEqual(input.Words()));
		}

		[TestMethod]
		public void Words_Mix()
		{
			const string input = "a    b\tc\r\nd\t\te\nf g h";
			Assert.IsTrue(new[] { "a", "b", "c", "d", "e", "f", "g", "h" }.SequenceEqual(input.Words()));
		}
	}
}
