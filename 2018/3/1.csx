using System.Collections;
using System.Text.RegularExpressions;

record Coord(long x, long y);
readonly struct Claim : IEnumerable<Coord>
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

	public IEnumerator<Coord> GetEnumerator()
	{
		for (long y = 0; y < Height; y++)
		{
			for (long x = 0; x < Width; x++)
			{
				yield return new(X + x, Y + y);
			}
		}
	}

	IEnumerator IEnumerable.GetEnumerator() =>
		GetEnumerator();
}

HashSet<Coord> once = new HashSet<Coord>();
HashSet<Coord> overlapping = new HashSet<Coord>();
foreach (Claim claim in File.ReadLines("input.txt").Select(Claim.Parse))
{
	foreach (Coord coord in claim)
	{
		if (!once.Add(coord))
		{
			overlapping.Add(coord);
		}
	}
}
WriteLine(overlapping.Count);
