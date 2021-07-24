using System.Text.RegularExpressions;

record Coord(long x, long y, long z);
static long ManhattanDistance(this Coord a, Coord b) =>
	Math.Abs(a.x - b.x) + Math.Abs(a.y - b.y) + Math.Abs(a.z - b.z);

Regex regex = new Regex(@"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)");
Coord maxRangeCoord = new Coord(0, 0, 0);
long maxRange = 0;
List<Coord> bots = new List<Coord>();
foreach (string line in File.ReadLines("input.txt"))
{
	Match match = regex.Match(line);
	Coord coord = new Coord(Int64.Parse(match.Groups[1].Value), Int64.Parse(match.Groups[2].Value), Int64.Parse(match.Groups[3].Value));
	bots.Add(coord);
	long range = Int64.Parse(match.Groups[4].Value);
	if (maxRange < range)
	{
		maxRangeCoord = coord;
		maxRange = range;
	}
}

WriteLine(bots.Count(c => maxRangeCoord.ManhattanDistance(c) <= maxRange));
