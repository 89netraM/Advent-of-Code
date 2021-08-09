using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using FsCheck;
using System.Linq;

namespace AoC.Library.Test
{
	[TestClass]
	public class Vector3Test
	{
		public static Arbitrary<Vector3> ArbitraryVector3 { get; } =
			Arb.From<Tuple<long, long, long>>().Convert(static p => new Vector3(p.Item1, p.Item2, p.Item3), static v => new(v.X, v.Y, v.Z));
		public static Arbitrary<Vector3> ArbitraryHexagonalVector3 { get; } =
			ArbitraryVector3.Filter(static v => v.IsHexagonal());

		[TestMethod]
		public void NeighborsMoore_MaximumIsRange()
		{
			Prop.ForAll(
					ArbitraryVector3,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsMoore(r).All(n => Math.Abs(n.X - v.X) <= r && Math.Abs(n.Y - v.Y) <= r && Math.Abs(n.Z - v.Z) <= r)
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsMoore_Distinct()
		{
			Prop.ForAll(
					ArbitraryVector3,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsMoore(r).Distinct().Count() == v.NeighborsMoore(r).Count()
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsMoore_NeighborCount()
		{
			Prop.ForAll(
					ArbitraryVector3,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsMoore(r).Count() == (int)Math.Pow(1 + 2 * r, 3) - 1
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsMoore_IsOrdered()
		{
			Prop.ForAll(
					ArbitraryVector3,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsMoore(r).Zip(v.NeighborsMoore(r).Skip(1), (a, b) => (a, b)).All(t => t.a < t.b)
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void NeighborsVonNeumann_InManhattanRange()
		{
			Prop.ForAll(
					ArbitraryVector3,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsVonNeumann(r).All(n => v.ManhattanDistance(n) <= r)
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsVonNeumann_Distinct()
		{
			Prop.ForAll(
					ArbitraryVector3,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsVonNeumann(r).Distinct().Count() == v.NeighborsVonNeumann(r).Count()
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsVonNeumann_NeighborCount()
		{
			Prop.ForAll(
					ArbitraryVector3,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsVonNeumann(r).Count() == Utils.DelannoyNumber(3, r) - 1
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void NeighborsVonNeumann_IsOrdered()
		{
			Prop.ForAll(
					ArbitraryVector3,
					Utils.PositiveLong,
					static (v, r) => v.NeighborsVonNeumann(r).Zip(v.NeighborsVonNeumann(r).Skip(1), (a, b) => (a, b)).All(t => t.a < t.b)
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void HexagonalRing_DistanceIsRange()
		{
			Prop.ForAll(
					ArbitraryHexagonalVector3,
					Utils.PositiveLong,
					static (v, r) => v.HexagonalRing(r).All(n => v.HexagonalManhattanDistance(n) == r)
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void HexagonalRing_Distinct()
		{
			Prop.ForAll(
					ArbitraryHexagonalVector3,
					Utils.PositiveLong,
					static (v, r) => v.HexagonalRing(r).Distinct().Count() == v.HexagonalRing(r).Count()
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void HexagonalRing_NeighborCount()
		{
			Prop.ForAll(
					ArbitraryHexagonalVector3,
					Utils.PositiveLong,
					static (v, r) => v.HexagonalRing(r).Count() == 6 * r
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void HexagonalNeighbors_MaximumIsRange()
		{
			Prop.ForAll(
					ArbitraryHexagonalVector3,
					Utils.PositiveLong,
					static (v, r) => v.HexagonalNeighbors(r).All(n => v.HexagonalManhattanDistance(n) <= r)
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void HexagonalNeighbors_Distinct()
		{
			Prop.ForAll(
					ArbitraryHexagonalVector3,
					Utils.PositiveLong,
					static (v, r) => v.HexagonalNeighbors(r).Distinct().Count() == v.HexagonalNeighbors(r).Count()
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void HexagonalNeighbors_NeighborCount()
		{
			Prop.ForAll(
					ArbitraryHexagonalVector3,
					Utils.PositiveLong,
					static (v, r) => v.HexagonalNeighbors(r).Count() == 3 * r * (r + 1)
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void ManhattanDistance()
		{
			Assert.AreEqual(0, Vector.Zero<Vector3>().ManhattanDistance(Vector.Zero<Vector3>()));
			Assert.AreEqual(3, Vector.Zero<Vector3>().ManhattanDistance(new Vector3(1, 1, 1)));
			Assert.AreEqual(3, Vector.Zero<Vector3>().ManhattanDistance(new Vector3(1, -1, -1)));
			Assert.AreEqual(9, new Vector3(12, 1, -4).ManhattanDistance(new Vector3(13, -2, 1)));
		}
		[TestMethod]
		public void Distance()
		{
			Assert.AreEqual(0.0000d, Vector.Zero<Vector3>().Distance(Vector.Zero<Vector3>()), 0.0001d);
			Assert.AreEqual(1.7321d, Vector.Zero<Vector3>().Distance(new Vector3(1, 1, 1)), 0.0001d);
			Assert.AreEqual(1.7321d, Vector.Zero<Vector3>().Distance(new Vector3(1, -1, -1)), 0.0001d);
			Assert.AreEqual(5.9161d, new Vector3(12, 1, -4).Distance(new Vector3(13, -2, 1)), 0.0001d);
		}

		[TestMethod]
		public void HexagonalManhattanDistance()
		{
			Assert.AreEqual(0, Vector.Zero<Vector3>().HexagonalManhattanDistance(Vector.Zero<Vector3>()));
			Assert.AreEqual(1, Vector.Zero<Vector3>().HexagonalManhattanDistance(new Vector3(1, 0, -1)));
			Assert.AreEqual(1, Vector.Zero<Vector3>().HexagonalManhattanDistance(new Vector3(1, -1, 0)));
			Assert.AreEqual(4, new Vector3(1, -2, 1).HexagonalManhattanDistance(new Vector3(2, 1, -3)));
		}

		[TestMethod]
		public void MinParts()
		{
			Prop.ForAll(
					ArbitraryVector3,
					ArbitraryVector3,
					static (a, b) =>
					{
						Vector3 min = a.MinParts(b);
						return min.X <= a.X && min.X <= b.X && min.Y <= a.Y && min.Y <= b.Y &&
							min.Z <= a.Z && min.Z <= b.Z;
					}
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void MaxParts()
		{
			Prop.ForAll(
					ArbitraryVector3,
					ArbitraryVector3,
					static (a, b) =>
					{
						Vector3 min = a.MaxParts(b);
						return min.X >= a.X && min.X >= b.X && min.Y >= a.Y && min.Y >= b.Y &&
							min.Z >= a.Z && min.Z >= b.Z;
					}
				).QuickCheckThrowOnFailure();
		}
	}
}
