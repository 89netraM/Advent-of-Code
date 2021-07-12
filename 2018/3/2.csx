using System.Collections;
using System.Text.RegularExpressions;

record Coord(long x, long y);
readonly struct Claim
{
	private static readonly Regex parsingRegex = new Regex(@"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)");
	public static Claim Parse(string line)
	{
		Match match = parsingRegex.Match(line);
		return new Claim(
			Int64.Parse(match.Groups[1].Value),
			Int64.Parse(match.Groups[2].Value),
			Int64.Parse(match.Groups[3].Value),
			Int64.Parse(match.Groups[4].Value),
			Int64.Parse(match.Groups[5].Value)
		);
	}

	public long Id { get; }
	public long X { get; }
	public long Y { get; }
	public long Width { get; }
	public long Height { get; }

	private Claim(long id, long x, long y, long width, long height) =>
		(Id, X, Y, Width, Height) = (id, x, y, width, height);

	public bool Overlapps(Claim other) =>
		!(X + Width <= other.X || other.X + other.Width <= X || // One is to the left
			Y + Height <= other.Y || other.Y + other.Height <= Y); // One is above

	public override int GetHashCode() =>
		HashCode.Combine(Id, X, Y, Width, Height);
}

List<Claim> previous = new List<Claim>();
HashSet<Claim> clear = new HashSet<Claim>();
foreach (Claim claim in File.ReadLines("input.txt").Select(Claim.Parse))
{
	bool anyOverlap = false;
	foreach (Claim prev in previous)
	{
		if (claim.Overlapps(prev))
		{
			clear.Remove(prev);
			anyOverlap = true;
		}
	}
	if (!anyOverlap)
	{
		clear.Add(claim);
	}
	previous.Add(claim);
}
WriteLine(String.Join(", ", clear.Select(static c => c.Id)));
