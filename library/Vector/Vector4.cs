using System;

namespace AoC.Library
{
	public struct Vector4 : IVector<Vector4>
	{
		long IVector<Vector4>.Count => 4;
		long IVector<Vector4>.this[long index]
		{
			get => index switch
			{
				0 => X,
				1 => Y,
				2 => Z,
				3 => T,
				_ => throw new IndexOutOfRangeException()
			};
			set
			{
				switch (index)
				{
					case 0:
						X = value;
						break;
					case 1:
						Y = value;
						break;
					case 2:
						Z = value;
						break;
					case 3:
						T = value;
						break;
					default:
						throw new IndexOutOfRangeException();
				}
			}
		}

		public long X { get; internal set; }
		public long Y { get; internal set; }
		public long Z { get; internal set; }
		public long T { get; internal set; }

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