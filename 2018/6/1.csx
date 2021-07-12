record Coord(int x, int y);

static int ManhattanDistance(Coord a, Coord b) =>
	Math.Abs(b.x - a.x) + Math.Abs(b.y - a.y);

Coord[] coords = File.ReadLines("input.txt")
	.Select(static l => l.Split(", "))
	.Select(static p => new Coord(Int32.Parse(p[0]), Int32.Parse(p[1])))
	.ToArray();
int[] numberOfCoordsClosestToCoord = new int[coords.Length];
Coord max = coords.Aggregate(static (a, c) => new(Math.Max(a.x, c.x), Math.Max(a.y, c.y)));

for (int y = -1; y <= max.y; y++)
{
	for (int x = -1; x <= max.x; x++)
	{
		Coord coord = new(x, y);
		var closest = coords.Select((c, i) => (d: ManhattanDistance(coord, c), i: (int?)i))
			.Aggregate(static (a, p) => a.d == p.d ? (a.d, null) : a.d < p.d ? a : p);
		if (closest.i is int index)
		{
			if (0 <= x && x < max.x && 0 <= y && y < max.y)
			{
				numberOfCoordsClosestToCoord[index]++;
			}
			else
			{
				numberOfCoordsClosestToCoord[index] = Int32.MinValue;
			}
		}
	}
}

WriteLine(numberOfCoordsClosestToCoord.Max());
