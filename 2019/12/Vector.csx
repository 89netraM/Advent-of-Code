readonly struct Vector : IEquatable<Vector>
{
	public static Vector Zero { get; } = default;
	public static Vector One { get; } = new Vector(1, 1, 1);
	public static Vector Right { get; } = new Vector(1, 0, 0);
	public static Vector Up { get; } = new Vector(0, 1, 0);
	public static Vector Back { get; } = new Vector(0, 0, 1);

	public static bool TryParse(string s, out Vector v)
	{
		s = s.Replace(" ", "").ToUpperInvariant();
		if (s.StartsWith("<") && s.EndsWith(">"))
		{
			int? x = null;
			int? y = null;
			int? z = null;

			foreach (string[] u in s[1..^1].Split(",").Select(u => u.Split("=")))
			{
				if (Int32.TryParse(u[1], out int i))
				{
					switch (u[0])
					{
						case "X":
							x = i;
							break;
						case "Y":
							y = i;
							break;
						case "Z":
							z = i;
							break;
						default:
							v = default;
							return false;
					}
				}
				else
				{
					v = default;
					return false;
				}
			}

			if (x.HasValue && y.HasValue && z.HasValue)
			{
				v = new Vector(x.Value, y.Value, z.Value);
				return true;
			}
			else
			{
				v = default;
				return false;
			}
		}
		else
		{
			v = default;
			return false;
		}
	}
	public static Vector Parse(string s)
	{
		if (TryParse(s, out Vector v))
		{
			return v;
		}
		else
		{
			throw new FormatException();
		}
	}

	public int X { get; }
	public int Y { get; }
	public int Z { get; }

	public Vector(int x, int y, int z) => (X, Y, Z) = (x, y, z);

	public Vector WithX(int x) => new Vector(x, Y, Z);
	public Vector WithY(int y) => new Vector(X, y, Z);
	public Vector WithZ(int z) => new Vector(X, Y, z);

	public int CalculateEnergy() => Math.Abs(X) + Math.Abs(Y) + Math.Abs(Z);

	public override string ToString() => $"<x={X}, y={Y}, z={Z}>";

	public bool Equals(Vector o) => X == o.X && Y == o.Y && Z == o.Z;
	public override bool Equals(Object o) => o is Vector v && Equals(v);
	public override int GetHashCode() => HashCode.Combine(X, Y, Z);
	public static bool operator ==(Vector a, Vector b) => a.Equals(b);
	public static bool operator !=(Vector a, Vector b) => !(a == b);

	public static Vector operator -(Vector v) => new Vector(-v.X, -v.Y, -v.Z);
	public static Vector operator +(Vector a, Vector b) => new Vector(a.X + b.X, a.Y + b.Y, a.Z + b.Z);
	public static Vector operator -(Vector a, Vector b) => new Vector(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
	public static Vector operator *(int s, Vector v) => v * s;
	public static Vector operator *(Vector v, int s) => new Vector(v.X * s, v.Y * s, v.Z * s);
}