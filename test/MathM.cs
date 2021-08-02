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
	}
}
