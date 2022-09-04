using System.Collections.Generic;
using FsCheck;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace AoC.Library.Test
{
	[TestClass]
	public class UOPTest
	{
		[TestMethod]
		public void GetHashCode_ShouldBeEqualForSameOrder()
		{
			Prop.ForAll(
					Arb.From<int>(),
					Arb.From<int>(),
					(a, b) =>
						UOP.New(a, b).GetHashCode() == UOP.New(a, b).GetHashCode()
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void GetHashCode_ShouldBeEqualForDifferentOrder()
		{
			Prop.ForAll(
					Arb.From<int>(),
					Arb.From<int>(),
					(a, b) =>
						UOP.New(a, b).GetHashCode() == UOP.New(b, a).GetHashCode()
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void Equals_ShouldBeTrueForSameOrder()
		{
			Prop.ForAll(
					Arb.From<int>(),
					Arb.From<int>(),
					(a, b) => UOP.New(a, b) == UOP.New(a, b)
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void Equals_ShouldBeTrueForDifferentOrder()
		{
			Prop.ForAll(
					Arb.From<int>(),
					Arb.From<int>(),
					(a, b) => UOP.New(a, b) == UOP.New(b, a)
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void Equals_ShouldBeTrueWhenBothIsNull()
		{
			UOP<int> a = null!;
			UOP<int> b = null!;
			Assert.IsTrue(a == b);
		}

		[TestMethod]
		public void Equals_ShouldBeFalseWhenAIsNull()
		{
			Prop.ForAll(
					Arb.From<UOP<int>>(),
					b => (null == b) == false
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void Equals_ShouldBeFalseWhenBIsNull()
		{
			Prop.ForAll(
					Arb.From<UOP<int>>(),
					a => (a == null) == false
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void NotEquals_ShouldBeFalseWhenBothIsNull()
		{
			UOP<int> a = null!;
			UOP<int> b = null!;
			Assert.IsFalse(a != b);
		}

		[TestMethod]
		public void NotEquals_ShouldBeTrueWhenAIsNull()
		{
			Prop.ForAll(
					Arb.From<UOP<int>>(),
					b => (null != b) == true
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void NotEquals_ShouldBeTrueWhenBIsNull()
		{
			Prop.ForAll(
					Arb.From<UOP<int>>(),
					a => (a != null) == true
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void Set_ShouldOccupySamePlaceInASetWithDifferentOrder()
		{
			Prop.ForAll(
					Arb.From<int>(),
					Arb.From<int>(),
					(a, b) => {
						ISet<UOP<int>> set = new HashSet<UOP<int>>();
						return set.Add(UOP.New(a, b)) &&
							!set.Add(UOP.New(b, a));
					}
				).QuickCheckThrowOnFailure();
		}
	}
}
