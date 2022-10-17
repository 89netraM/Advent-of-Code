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
		public void Rotate_StandardDirectionsFromUp()
		{
			Assert.AreEqual(Vector2.Right, Vector2.Up.Rotate(1));
			Assert.AreEqual(Vector2.Down, Vector2.Up.Rotate(2));
			Assert.AreEqual(Vector2.Left, Vector2.Up.Rotate(3));
			Assert.AreEqual(Vector2.Up, Vector2.Up.Rotate(4));

			Assert.AreEqual(Vector2.Left, Vector2.Up.Rotate(-1));
			Assert.AreEqual(Vector2.Down, Vector2.Up.Rotate(-2));
			Assert.AreEqual(Vector2.Right, Vector2.Up.Rotate(-3));
			Assert.AreEqual(Vector2.Up, Vector2.Up.Rotate(-4));
		}
		[TestMethod]
		public void Rotate_StandardDirectionsFromRight()
		{
			Assert.AreEqual(Vector2.Down, Vector2.Right.Rotate(1));
			Assert.AreEqual(Vector2.Left, Vector2.Right.Rotate(2));
			Assert.AreEqual(Vector2.Up, Vector2.Right.Rotate(3));
			Assert.AreEqual(Vector2.Right, Vector2.Right.Rotate(4));

			Assert.AreEqual(Vector2.Up, Vector2.Right.Rotate(-1));
			Assert.AreEqual(Vector2.Left, Vector2.Right.Rotate(-2));
			Assert.AreEqual(Vector2.Down, Vector2.Right.Rotate(-3));
			Assert.AreEqual(Vector2.Right, Vector2.Right.Rotate(-4));
		}
		[TestMethod]
		public void Rotate_StandardDirectionsFromDown()
		{
			Assert.AreEqual(Vector2.Left, Vector2.Down.Rotate(1));
			Assert.AreEqual(Vector2.Up, Vector2.Down.Rotate(2));
			Assert.AreEqual(Vector2.Right, Vector2.Down.Rotate(3));
			Assert.AreEqual(Vector2.Down, Vector2.Down.Rotate(4));

			Assert.AreEqual(Vector2.Right, Vector2.Down.Rotate(-1));
			Assert.AreEqual(Vector2.Up, Vector2.Down.Rotate(-2));
			Assert.AreEqual(Vector2.Left, Vector2.Down.Rotate(-3));
			Assert.AreEqual(Vector2.Down, Vector2.Down.Rotate(-4));
		}
		[TestMethod]
		public void Rotate_StandardDirectionsFromLeft()
		{
			Assert.AreEqual(Vector2.Up, Vector2.Left.Rotate(1));
			Assert.AreEqual(Vector2.Right, Vector2.Left.Rotate(2));
			Assert.AreEqual(Vector2.Down, Vector2.Left.Rotate(3));
			Assert.AreEqual(Vector2.Left, Vector2.Left.Rotate(4));

			Assert.AreEqual(Vector2.Down, Vector2.Left.Rotate(-1));
			Assert.AreEqual(Vector2.Right, Vector2.Left.Rotate(-2));
			Assert.AreEqual(Vector2.Up, Vector2.Left.Rotate(-3));
			Assert.AreEqual(Vector2.Left, Vector2.Left.Rotate(-4));
		}
		[TestMethod]
		public void Rotate_Reversible()
		{
			Prop.ForAll(
					ArbitraryVector2,
					Arb.From<long>(),
					static (v, q) => v.Rotate(q).Rotate(-q) == v
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void Rotate_PreservesLength()
		{
			Prop.ForAll(
					ArbitraryVector2,
					Arb.From<long>(),
					static (v, q) => Vector2.Zero.Distance(v.Rotate(q)) == Vector2.Zero.Distance(v)
				).QuickCheckThrowOnFailure();
		}

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

		[TestMethod]
		public void MinParts()
		{
			Prop.ForAll(
					ArbitraryVector2,
					ArbitraryVector2,
					static (a, b) =>
					{
						Vector2 min = a.MinParts(b);
						return min.X <= a.X && min.X <= b.X && min.Y <= a.Y && min.Y <= b.Y;
					}
				).QuickCheckThrowOnFailure();
		}
		[TestMethod]
		public void MaxParts()
		{
			Prop.ForAll(
					ArbitraryVector2,
					ArbitraryVector2,
					static (a, b) =>
					{
						Vector2 min = a.MaxParts(b);
						return min.X >= a.X && min.X >= b.X && min.Y >= a.Y && min.Y >= b.Y;
					}
				).QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void Deconstructor()
		{
			const long X = 1;
			const long Y = 2;

			var (x, y) = new Vector2(X, Y);
			Assert.AreEqual(X, x);
			Assert.AreEqual(Y, y);
		}
	}
}
