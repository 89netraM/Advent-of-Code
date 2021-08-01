using System;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace AoC.Library.Test
{
	public partial class ParsingTest
	{
		[TestMethod]
		public void ParseParentheses_NoParentheses()
		{
			Assert.IsFalse(Parsing.ParseParentheses("hello world").Any());
		}

		[TestMethod]
		public void ParseParentheses_OneLevelOfParentheses()
		{
			Assert.IsTrue(
				Parsing.ParseParentheses("(hello) (world)")
					.SequenceEqual(new[] { "hello", "world" })
			);
		}

		[TestMethod]
		public void ParseParentheses_MultipleLevelsOfParentheses()
		{
			Assert.IsTrue(
				Parsing.ParseParentheses("(h(e(ll)o)) ((world))")
					.SequenceEqual(new[] { "h(e(ll)o)", "(world)" })
			);
		}

		[TestMethod]
		public void ParseParentheses_EmptyParentheses()
		{
			Assert.IsTrue(
				Parsing.ParseParentheses("()")
					.SequenceEqual(new[] { "" })
			);
		}

		[TestMethod]
		public void ParseDeepParentheses_MultipleLevelsOfParentheses()
		{
			Assert.IsTrue(
				Parsing.ParseDeepParentheses("(h(e(ll)o)) ((world))")
					.SequenceEqual(new[] { "h(e(ll)o)", "e(ll)o", "ll", "(world)", "world" })
			);
		}
	}
}
