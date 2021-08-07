using System;
using System.Collections;
using System.Collections.Generic;

namespace AoC.Library
{
	public readonly struct Vector4 : IVector<Vector4>
	{
		long IVector<Vector4>.Count => 4;
		long IVector<Vector4>.this[long index] => index switch
		{
			0 => X,
			1 => Y,
			2 => Z,
			3 => T,
			_ => throw new IndexOutOfRangeException()
		};

		public long X { get; }
		public long Y { get; }
		public long Z { get; }
		public long T { get; }

		public Vector4(long x, long y, long z, long t) =>
			(X, Y, Z, T) = (x, y, z, t);

		public bool Equals(Vector4 other) =>
			Vector.Equals(this, other);
		public override bool Equals(object? obj) =>
			obj is Vector4 other && Equals(other);
		public override int GetHashCode() =>
			HashCode.Combine(X, Y, Z, T);
		public int CompareTo(Vector4 other) =>
			Vector.Compare(this, other);

		public override string ToString() =>
			Vector.AsString(this);

		public IEnumerator<long> GetEnumerator() =>
			((IEnumerable<long>)new[] { X, Y, Z, T }).GetEnumerator();
		IEnumerator IEnumerable.GetEnumerator() =>
			GetEnumerator();

		public static Vector4 operator +(Vector4 a, Vector4 b) =>
			a.Add(b);
		public static Vector4 operator -(Vector4 a, Vector4 b) =>
			a.Subtract(b);
		public static Vector4 operator *(Vector4 a, long value) =>
			a.Multiply(value);
		public static Vector4 operator /(Vector4 a, long value) =>
			a.Divide(value);
		public static Vector4 operator -(Vector4 a) =>
			a.Negate();

		public static bool operator <(Vector4 a, Vector4 b) =>
			a.CompareTo(b) < 0;
		public static bool operator >(Vector4 a, Vector4 b) =>
			a.CompareTo(b) > 0;
		public static bool operator <=(Vector4 a, Vector4 b) =>
			a.CompareTo(b) <= 0;
		public static bool operator >=(Vector4 a, Vector4 b) =>
			a.CompareTo(b) >= 0;
		public static bool operator ==(Vector4 a, Vector4 b) =>
			a.Equals(b);
		public static bool operator !=(Vector4 a, Vector4 b) =>
			!a.Equals(b);
	}
}
