using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using FsCheck;
using System.Linq;

namespace AoC.Library.Test
{
	[Ignore]
	[TestClass]
	public class Vector4Test
	{
		public static Arbitrary<Vector4> ArbitraryVector4 { get; } =
			Arb.From<Tuple<long, long, long, long>>().Convert(static p => new Vector4(p.Item1, p.Item2, p.Item3, p.Item4), static v => new(v.X, v.Y, v.Z, v.T));

		[TestMethod]
		public void NeighborsMoore_MaximumIsRange()
		{
			Prop.ForAll(
					ArbitraryVector4,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsMoore(r).All(n => Math.Abs(n.X - v.X) <= r && Math.Abs(n.Y - v.Y) <= r && Math.Abs(n.Z - v.Z) <= r && Math.Abs(n.T - v.T) <= r)
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsMoore_Distinct()
		{
			Prop.ForAll(
					ArbitraryVector4,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsMoore(r).Distinct().Count() == v.NeighborsMoore(r).Count()
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsMoore_NeighborCount()
		{
			Prop.ForAll(
					ArbitraryVector4,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsMoore(r).Count() == (int)Math.Pow(1 + 2 * r, 4) - 1
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsMoore_IsOrdered()
		{
			Prop.ForAll(
					ArbitraryVector4,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsMoore(r).Zip(v.NeighborsMoore(r).Skip(1), (a, b) => (a, b)).All(t => t.a < t.b)
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void NeighborsVonNeumann_InManhattanRange()
		{
			Prop.ForAll(
					ArbitraryVector4,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsVonNeumann(r).All(n => v.ManhattanDistance(n) <= r)
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsVonNeumann_Distinct()
		{
			Prop.ForAll(
					ArbitraryVector4,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsVonNeumann(r).Distinct().Count() == v.NeighborsVonNeumann(r).Count()
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsVonNeumann_NeighborCount()
		{
			Prop.ForAll(
					ArbitraryVector4,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsVonNeumann(r).Count() == Utils.DelannoyNumber(4, r) - 1
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsVonNeumann_IsOrdered()
		{
			Prop.ForAll(
					ArbitraryVector4,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsVonNeumann(r).Zip(v.NeighborsVonNeumann(r).Skip(1), (a, b) => (a, b)).All(t => t.a < t.b)
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void ManhattanDistance()
		{
			Assert.AreEqual(0, Vector.Zero<Vector4>().ManhattanDistance(Vector.Zero<Vector4>()));
			Assert.AreEqual(4, Vector.Zero<Vector4>().ManhattanDistance(new Vector4(1, 1, 1, 1)));
			Assert.AreEqual(4, Vector.Zero<Vector4>().ManhattanDistance(new Vector4(1, -1, -1, 1)));
			Assert.AreEqual(13, new Vector4(12, 1, -4, -1).ManhattanDistance(new Vector4(13, -2, 1, -5)));
		}
		[TestMethod]
		public void Distance()
		{
			Assert.AreEqual(0.0000d, Vector.Zero<Vector4>().Distance(Vector.Zero<Vector4>()), 0.0001d);
			Assert.AreEqual(2.0000d, Vector.Zero<Vector4>().Distance(new Vector4(1, 1, 1, 1)), 0.0001d);
			Assert.AreEqual(2.0000d, Vector.Zero<Vector4>().Distance(new Vector4(1, -1, -1, 1)), 0.0001d);
			Assert.AreEqual(7.1414d, new Vector4(12, 1, -4, -1).Distance(new Vector4(13, -2, 1, -5)), 0.0001d);
		}

		[TestMethod]
		public void MinParts()
		{
			Prop.ForAll(
					ArbitraryVector4,
					ArbitraryVector4,
					static (a, b) =>
					{
						Vector4 min = a.MinParts(b);
						return min.X <= a.X && min.X <= b.X && min.Y <= a.Y && min.Y <= b.Y &&
							min.Z <= a.Z && min.Z <= b.Z && min.T <= a.T && min.T <= b.T;
					}
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void MaxParts()
		{
			Prop.ForAll(
					ArbitraryVector4,
					ArbitraryVector4,
					static (a, b) =>
					{
						Vector4 min = a.MaxParts(b);
						return min.X >= a.X && min.X >= b.X && min.Y >= a.Y && min.Y >= b.Y &&
							min.Z >= a.Z && min.Z >= b.Z && min.T >= a.T && min.T >= b.T;
					}
				).QuickCheckThrowOnFailure();
		}
	}
}
