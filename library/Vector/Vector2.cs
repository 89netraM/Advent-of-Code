using System;
using System.Collections;
using System.Collections.Generic;

namespace AoC.Library
{
	public readonly struct Vector2 : IVector<Vector2>
	{
		long IVector<Vector2>.Count => 2;
		long IVector<Vector2>.this[long index] => index switch
		{
			0 => X,
			1 => Y,
			_ => throw new IndexOutOfRangeException()
		};

		public long X { get; }
		public long Y { get; }

		public Vector2(long x, long y) =>
			(X, Y) = (x, y);

		public bool Equals(Vector2 other) =>
			Vector.Equals(this, other);
		public override bool Equals(object? obj) =>
			obj is Vector2 other && Equals(other);
		public override int GetHashCode() =>
			HashCode.Combine(X, Y);
		public int CompareTo(Vector2 other) =>
			Vector.Compare(this, other);

		public override string ToString() =>
			Vector.AsString(this);

		public IEnumerator<long> GetEnumerator() =>
			((IEnumerable<long>)new[] { X, Y }).GetEnumerator();
		IEnumerator IEnumerable.GetEnumerator() =>
			GetEnumerator();

		public static Vector2 operator +(Vector2 a, Vector2 b) =>
			a.Add(b);
		public static Vector2 operator -(Vector2 a, Vector2 b) =>
			a.Subtract(b);
		public static Vector2 operator *(Vector2 a, long value) =>
			a.Multiply(value);
		public static Vector2 operator /(Vector2 a, long value) =>
			a.Divide(value);
		public static Vector2 operator -(Vector2 a) =>
			a.Negate();

		public static bool operator <(Vector2 a, Vector2 b) =>
			a.CompareTo(b) < 0;
		public static bool operator >(Vector2 a, Vector2 b) =>
			a.CompareTo(b) > 0;
		public static bool operator <=(Vector2 a, Vector2 b) =>
			a.CompareTo(b) <= 0;
		public static bool operator >=(Vector2 a, Vector2 b) =>
			a.CompareTo(b) >= 0;
		public static bool operator ==(Vector2 a, Vector2 b) =>
			a.Equals(b);
		public static bool operator !=(Vector2 a, Vector2 b) =>
			!a.Equals(b);
	}
}
