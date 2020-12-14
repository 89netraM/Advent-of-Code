#load ".\Vector.csx"

using System.Text.RegularExpressions;

readonly struct Body : IEquatable<Body>
{
	private static readonly Regex parser = new Regex(@"(?<prop>\w+)=(?<vec><.*?>),?", RegexOptions.Compiled);
	public static bool TryParse(string s, out Body b)
	{
		s = s.Replace(" ", "").ToUpperInvariant();
		Vector? position = null;
		Vector? velocity = null;

		foreach (Match m in parser.Matches(s))
		{
			if (Vector.TryParse(m.Groups["vec"].Value, out Vector v))
			{
				switch (m.Groups["prop"].Value)
				{
					case "POS":
						position = v;
						break;
					case "VEL":
						velocity = v;
						break;
					default:
						b = default;
						return false;
				}
			}
			else
			{
				b = default;
				return false;
			}
		}

		if (position.HasValue)
		{
			b = new Body(position.Value, velocity ?? default);
			return true;
		}
		else
		{
			b = default;
			return false;
		}
	}
	public static Body Parse(string s)
	{
		if (TryParse(s, out Body b))
		{
			return b;
		}
		else
		{
			throw new FormatException();
		}
	}

	public Vector Position { get; }
	public Vector Velocity { get; }

	public Body(Vector position) : this(position, default) { }
	public Body(Vector position, Vector velocity) => (Position, Velocity) = (position, velocity);

	public Body WithPosition(Vector position) => new Body(position, Velocity);
	public Body WithVelocity(Vector velocity) => new Body(Position, velocity);

	public int CalculateEnergy() => Position.CalculateEnergy() * Velocity.CalculateEnergy();

	public Body Drag(IEnumerable<Body> others)
	{
		Body b = this;
		return WithVelocity(
			b.Velocity +
			others.Aggregate(
				Vector.Zero,
				(a, o) => a + new Vector(
					b.Position.X < o.Position.X ? 1 : b.Position.X > o.Position.X ? -1 : 0,
					b.Position.Y < o.Position.Y ? 1 : b.Position.Y > o.Position.Y ? -1 : 0,
					b.Position.Z < o.Position.Z ? 1 : b.Position.Z > o.Position.Z ? -1 : 0
				)
			)
		);
	}

	public Body Step() => WithPosition(Position + Velocity);

	public override string ToString()
	{
		string s = $"pos={Position}";
		if (Velocity != default)
		{
			s += $", vel={Velocity}";
		}
		return s;
	}

	public bool Equals(Body o) => Position == o.Position && Velocity == o.Velocity;
	public override bool Equals(Object o) => o is Body b && Equals(b);
	public override int GetHashCode() => HashCode.Combine(Position, Velocity);
	public static bool operator ==(Body a, Body b) => a.Equals(b);
	public static bool operator !=(Body a, Body b) => !(a == b);
}