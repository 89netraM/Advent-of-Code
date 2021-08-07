using System;
using System.Collections;
using System.Collections.Generic;

namespace AoC.Library
{
	public readonly struct Vector3 : IVector<Vector3>
	{
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
