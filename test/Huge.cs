using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace AoC.Library.Test
{
	[TestClass]
	public class HugeTest
	{
		[TestMethod]
		public void Huge_BasicPeriod()
		{
			const long TotalSteps = 1_000_000_000L;
			long[] values = new long[] { 12L, 50L, 1L, 4L, 10L, 99L, 5L, 7L, 0L, 33L };
			long stepsTaken = 0L;

			long state = 0L;
			long result = Huge.TakeSteps(
				TotalSteps,
				i =>
				{
					stepsTaken++;
					long prevState = state;
					state = MathM.Mod(state + 1, values.Length);
					return values[prevState];
				}
			);

			Assert.AreEqual(values.Length + 1L, stepsTaken);
			Assert.AreEqual(values[0], result);
		}

		[TestMethod]
		public void Huge_BasicArithmeticProgression()
		{
			long[] values = new long[] { 0L, 2L, 5L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L };
			long stepsTaken = 0L;

			long state = 0L;
			long result = Huge.TakeSteps(
				values.Length,
				i =>
				{
					stepsTaken++;
					return values[state++];
				},
				HugeSearchPattern.Progressions
			);

			Assert.IsTrue(stepsTaken < values.Length);
			Assert.AreEqual(values[^1], result);
		}

		[TestMethod]
		public void Huge_BasicGeometricProgression()
		{
			long[] values = new long[] { 0L, 2L, 5L, 7L, 14L, 28L, 56L, 112L, 224L, 448L, 896L, 1_792L };
			long stepsTaken = 0L;

			long state = 0L;
			long result = Huge.TakeSteps(
				values.Length,
				i =>
				{
					stepsTaken++;
					return values[state++];
				}
			);

			Assert.IsTrue(stepsTaken < values.Length);
			Assert.AreEqual(values[^1], result);
		}
	}
}
