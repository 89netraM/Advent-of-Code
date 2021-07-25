using System;

namespace AoC.Library
{
	public struct Vector3 : IVector<Vector3>
	{
		long IVector<Vector3>.Count => 3;
		long IVector<Vector3>.this[long index]
		{
			get => index switch
			{
				0 => X,
				1 => Y,
				2 => Z,
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
					default:
						throw new IndexOutOfRangeException();
				}
			}
		}

		public long X { get; internal set; }
		public long Y { get; internal set; }
		public long Z { get; internal set; }

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
