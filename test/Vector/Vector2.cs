using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using FsCheck;
using System.Linq;

namespace AoC.Library.Test
{
	[TestClass]
	public class Vector2Test
	{
		public static Arbitrary<Vector2> ArbitraryVector2 { get; } =
			Arb.From<Tuple<long, long>>().Convert(static p => new Vector2(p.Item1, p.Item2), static v => new(v.X, v.Y));

		[TestMethod]
		public void NeighborsMoore_MaximumIsRange()
		{
			Prop.ForAll(
					ArbitraryVector2,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsMoore(r).All(n => Math.Abs(n.X - v.X) <= r && Math.Abs(n.Y - v.Y) <= r)
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsMoore_Distinct()
		{
			Prop.ForAll(
					ArbitraryVector2,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsMoore(r).Distinct().Count() == v.NeighborsMoore(r).Count()
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsMoore_NeighborCount()
		{
			Prop.ForAll(
					ArbitraryVector2,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsMoore(r).Count() == (int)Math.Pow(1 + 2 * r, 2) - 1
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsMoore_IsOrdered()
		{
			Prop.ForAll(
					ArbitraryVector2,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsMoore(r).Zip(v.NeighborsMoore(r).Skip(1), (a, b) => (a, b)).All(t => t.a < t.b)
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void NeighborsVonNeumann_InManhattanRange()
		{
			Prop.ForAll(
					ArbitraryVector2,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsVonNeumann(r).All(n => v.ManhattanDistance(n) <= r)
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsVonNeumann_Distinct()
		{
			Prop.ForAll(
					ArbitraryVector2,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsVonNeumann(r).Distinct().Count() == v.NeighborsVonNeumann(r).Count()
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsVonNeumann_NeighborCount()
		{
			Prop.ForAll(
					ArbitraryVector2,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsVonNeumann(r).Count() == Utils.DelannoyNumber(2, r) - 1
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsVonNeumann_IsOrdered()
		{
			Prop.ForAll(
					ArbitraryVector2,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsVonNeumann(r).Zip(v.NeighborsVonNeumann(r).Skip(1), (a, b) => (a, b)).All(t => t.a < t.b)
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void ManhattanDistance()
		{
			Assert.AreEqual(0, Vector.Zero<Vector2>().ManhattanDistance(Vector.Zero<Vector2>()));
			Assert.AreEqual(2, Vector.Zero<Vector2>().ManhattanDistance(new Vector2(1, 1)));
			Assert.AreEqual(2, Vector.Zero<Vector2>().ManhattanDistance(new Vector2(1, -1)));
			Assert.AreEqual(4, new Vector2(12, 1).ManhattanDistance(new Vector2(13, -2)));
		}
		[TestMethod]
		public void Distance()
		{
			Assert.AreEqual(0.0000d, Vector.Zero<Vector2>().Distance(Vector.Zero<Vector2>()), 0.0001d);
			Assert.AreEqual(1.4142d, Vector.Zero<Vector2>().Distance(new Vector2(1, 1)), 0.0001d);
			Assert.AreEqual(1.4142d, Vector.Zero<Vector2>().Distance(new Vector2(1, -1)), 0.0001d);
			Assert.AreEqual(3.1623d, new Vector2(12, 1).Distance(new Vector2(13, -2)), 0.0001d);
		}
	}
}
