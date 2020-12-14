#load ".\Body.csx"

readonly struct Universe : IEquatable<Universe>
{
	public IEnumerable<Body> Bodies { get; }

	public Universe(IEnumerable<Body> bodies) => Bodies = bodies.ToList();

	public int CalculateEnergy() => Bodies.Sum(b => b.CalculateEnergy());

	public Universe Step()
	{
		IEnumerable<Body> bodies = Bodies;
		return new Universe(Bodies.Select(b => b.Drag(bodies).Step()));
	}

	public override string ToString() => String.Join("\n", Bodies);

	public bool Equals(Universe o) => Bodies.SequenceEqual(o.Bodies);
	public override bool Equals(Object o) => o is Universe u && Equals(u);
	public override int GetHashCode() => HashCode.Combine(Bodies);
	public static bool operator ==(Universe a, Universe b) => a.Equals(b);
	public static bool operator !=(Universe a, Universe b) => !(a == b);
}