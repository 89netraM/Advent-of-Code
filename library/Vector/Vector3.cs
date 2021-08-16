using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace AoC.Library
{
	public readonly struct Vector3 : IVector<Vector3>
	{
		public static Vector3 Zero { get; } = Vector.Zero<Vector3>();
		public static Vector3 UnitX { get; } = new Vector3(1, 0, 0);
		public static Vector3 UnitY { get; } = new Vector3(0, 1, 0);
		public static Vector3 UnitZ { get; } = new Vector3(0, 0, 1);
		public static Vector3 Up { get; } = new Vector3(0, -1, 0);
		public static Vector3 Down { get; } = new Vector3(0, 1, 0);
		public static Vector3 Left { get; } = new Vector3(-1, 0, 0);
		public static Vector3 Right { get; } = new Vector3(1, 0, 0);
		public static Vector3 Forward { get; } = new Vector3(0, 0, -1);
		public static Vector3 Backward { get; } = new Vector3(0, 0, 1);

		public static Vector3 HexagonalFlatNorth { get; } = new Vector3(0, 1, -1);
		public static Vector3 HexagonalFlatNorthEast { get; } = new Vector3(1, 0, -1);
		public static Vector3 HexagonalFlatNorthWest { get; } = new Vector3(-1, 1, 0);
		public static Vector3 HexagonalFlatSouthEast { get; } = new Vector3(1, -1, 0);
		public static Vector3 HexagonalFlatSouthWest { get; } = new Vector3(-1, 0, 1);
		public static Vector3 HexagonalFlatSouth { get; } = new Vector3(0, -1, 1);

		public static Vector3 HexagonalPointyEast { get; } = HexagonalFlatSouthEast;
		public static Vector3 HexagonalPointyNorthEast { get; } = HexagonalFlatNorthEast;
		public static Vector3 HexagonalPointySouthEast { get; } = HexagonalFlatSouth;
		public static Vector3 HexagonalPointyNorthWest { get; } = HexagonalFlatNorth;
		public static Vector3 HexagonalPointySouthWest { get; } = HexagonalFlatSouthWest;
		public static Vector3 HexagonalPointyWest { get; } = HexagonalFlatNorthWest;

		public static IImmutableList<Vector3> HexagonalDirections { get; } = ImmutableArray.Create(
			HexagonalFlatSouthEast,
			HexagonalFlatNorthEast,
			HexagonalFlatNorth,
			HexagonalFlatNorthWest,
			HexagonalFlatSouthWest,
			HexagonalFlatSouth
		);

		long IVector<Vector3>.Count => 3;
		long IVector<Vector3>.this[long index] => index switch
		{
			0 => X,
			1 => Y,
			2 => Z,
			_ => throw new IndexOutOfRangeException()
		};

		public long X { get; }
		public long Y { get; }
		public long Z { get; }

		public Vector3(long x, long y, long z) =>
			(X, Y, Z) = (x, y, z);

		public IEnumerable<Vector3> HexagonalNeighbors(long range = 1)
		{
			for (long k = 1; k <= range; k++)
			{
				foreach (Vector3 v in HexagonalRing(k))
				{
					yield return v;
				}
			}
		}
		public IEnumerable<Vector3> HexagonalRing(long radius)
		{
			Vector3 cube = this + HexagonalDirections[4] * radius;

			for (int i = 0; i < 6; i++)
			{
				for (long j = 0; j < radius; j++)
				{
					yield return cube;
					cube += HexagonalDirections[i];
				}
			}
		}

		public long HexagonalManhattanDistance(Vector3 other) =>
			this.ManhattanDistance(other) / 2;

		public bool IsHexagonal() =>
			X + Y + Z == 0;

		public void Deconstruct(out long x, out long y, out long z) =>
			(x, y, z) = (X, Y, Z);

		public bool Equals(Vector3 other) =>
			Vector.Equals(this, other);
		public override bool Equals(object? obj) =>
			obj is Vector3 other && Equals(other);
		public override int GetHashCode() =>
			HashCode.Combine(X, Y, Z);
		public int CompareTo(Vector3 other) =>
			Vector.Compare(this, other);

		public override string ToString() =>
			Vector.AsString(this);

		public IEnumerator<long> GetEnumerator() =>
			((IEnumerable<long>)new[] { X, Y, Z }).GetEnumerator();
		IEnumerator IEnumerable.GetEnumerator() =>
			GetEnumerator();

		public static Vector3 operator +(Vector3 a, Vector3 b) =>
			a.Add(b);
		public static Vector3 operator -(Vector3 a, Vector3 b) =>
			a.Subtract(b);
		public static Vector3 operator *(Vector3 a, long value) =>
			a.Multiply(value);
		public static Vector3 operator /(Vector3 a, long value) =>
			a.Divide(value);
		public static Vector3 operator -(Vector3 a) =>
			a.Negate();

		public static bool operator <(Vector3 a, Vector3 b) =>
			a.CompareTo(b) < 0;
		public static bool operator >(Vector3 a, Vector3 b) =>
			a.CompareTo(b) > 0;
		public static bool operator <=(Vector3 a, Vector3 b) =>
			a.CompareTo(b) <= 0;
		public static bool operator >=(Vector3 a, Vector3 b) =>
			a.CompareTo(b) >= 0;
		public static bool operator ==(Vector3 a, Vector3 b) =>
			a.Equals(b);
		public static bool operator !=(Vector3 a, Vector3 b) =>
			!a.Equals(b);
	}
}
