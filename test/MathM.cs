using System.Collections.Generic;
using FsCheck;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace AoC.Library.Test
{
	[TestClass]
	public class MathMTest
	{
		[TestMethod]
		public void ModInt_Correct()
		{
			Assert.AreEqual(5, MathM.Mod(5, 10));
			Assert.AreEqual(5, MathM.Mod(-5, 10));
			Assert.AreEqual(0, MathM.Mod(10, 10));
			Assert.AreEqual(0, MathM.Mod(-10, 10));
		}

		[TestMethod]
		public void ModInt_ShouldBePositive()
		{
			Prop.ForAll(
					Arb.From<int>(),
					Utils.NonZeroInt,
					static (a, b) => MathM.Mod(a, b) >= 0
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void ModLong_Correct()
		{
			Assert.AreEqual(5L, MathM.Mod(5L, 10L));
			Assert.AreEqual(5L, MathM.Mod(-5L, 10L));
			Assert.AreEqual(0L, MathM.Mod(10L, 10L));
			Assert.AreEqual(0L, MathM.Mod(-10L, 10L));
		}

		[TestMethod]
		public void ModLong_ShouldBePositive()
		{
			Prop.ForAll(
					Arb.From<long>(),
					Utils.NonZeroLong,
					static (a, b) => MathM.Mod(a, b) >= 0L
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void Gcd_Correct()
		{
			Assert.AreEqual(6L, MathM.Gcd(54L, 24L));
			Assert.AreEqual(6L, MathM.Gcd(-54L, 24L));
			Assert.AreEqual(1L, MathM.Gcd(9L, 28L));
			Assert.AreEqual(12L, MathM.Gcd(24L, 60L));
		}

		[TestMethod]
		public void Gcd_ShouldBeStrictlyPositive()
		{
			Prop.ForAll(
					Utils.NonZeroLong,
					Arb.From<long>(),
					static (a, b) => MathM.Gcd(a, b) > 0L
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void Gcd_Params()
		{
			Assert.AreEqual(3L, MathM.Gcd(9L, 12L, 21L));
		}

		[TestMethod]
		public void Gcd_IEnumerable()
		{
			List<long> numbers = new List<long> { 9L, 12L, 21L };
			Assert.AreEqual(3L, MathM.Gcd(numbers));
			Assert.AreEqual(3L, numbers.Gcd());
		}

		[TestMethod]
		public void Lcm_Correct()
		{
			Assert.AreEqual(42L, MathM.Lcm(21L, 6L));
			Assert.AreEqual(42L, MathM.Lcm(21L, -6L));
			Assert.AreEqual(9L, MathM.Lcm(3L, 9L));
			Assert.AreEqual(40L, MathM.Lcm(8L, 10L));
		}

		[TestMethod]
		public void Lcm_ShouldBeStrictlyPositive()
		{
			Prop.ForAll(
					Utils.NonZeroLong,
					Utils.NonZeroLong,
					static (a, b) => MathM.Lcm(a, b) > 0L
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void Lcm_Params()
		{
			Assert.AreEqual(504L, MathM.Lcm(8L, 9L, 21L));
		}

		[TestMethod]
		public void Lcm_IEnumerable()
		{
			List<long> numbers = new List<long> { 8L, 9L, 21L };
			Assert.AreEqual(504L, MathM.Lcm(numbers));
			Assert.AreEqual(504L, numbers.Lcm());
		}
	}
}
