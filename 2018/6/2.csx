record Coord(int x, int y);

static int ManhattanDistance(Coord a, Coord b) =>
	Math.Abs(b.x - a.x) + Math.Abs(b.y - a.y);

Coord[] coords = File.ReadLines("input.txt")
	.Select(static l => l.Split(", "))
	.Select(static p => new Coord(Int32.Parse(p[0]), Int32.Parse(p[1])))
	.ToArray();
int closeCoords = 0;
Coord max = coords.Aggregate(static (a, c) => new(Math.Max(a.x, c.x), Math.Max(a.y, c.y)));

for (int y = 0; y < max.y; y++)
{
	for (int x = 0; x < max.x; x++)
	{
		Coord coord = new(x, y);
		if (coords.Sum(c => ManhattanDistance(c, coord)) < 10000)
		{
			closeCoords++;
		}
	}
}

WriteLine(closeCoords);
