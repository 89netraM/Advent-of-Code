using System;
using System.Collections;
using System.Collections.Generic;

namespace AoC.Library
{
	public readonly struct Vector2 : IVector<Vector2>
	{
		public static Vector2 Zero { get; } = Vector.Zero<Vector2>();
		public static Vector2 UnitX { get; } = new Vector2(1, 0);
		public static Vector2 UnitY { get; } = new Vector2(0, 1);
		public static Vector2 Up { get; } = new Vector2(0, -1);
		public static Vector2 Down { get; } = new Vector2(0, 1);
		public static Vector2 Left { get; } = new Vector2(-1, 0);
		public static Vector2 Right { get; } = new Vector2(1, 0);

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

		/// <summary>
		/// Returns a vector rotated the given number of <paramref name="quarters" /> around origo.
		/// </summary>
		public Vector2 Rotate(long quarters)
		{
			return new Vector2(
				X * qCos(quarters) - Y * qSin(quarters),
				X * qSin(quarters) + Y * qCos(quarters));

			#pragma warning disable CS8509
			static long qCos(long quarters) =>
				MathM.Mod(quarters, 4) switch
				{
					0 => 1,
					1 => 0,
					2 => -1,
					3 => 0,
				};

			static long qSin(long quarters) =>
				MathM.Mod(quarters, 4) switch
				{
					0 => 0,
					1 => 1,
					2 => 0,
					3 => -1,
				};
			#pragma warning restore CS8509
		}

		public void Deconstruct(out long x, out long y) =>
			(x, y) = (X, Y);

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
