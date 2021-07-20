record Coord(int x, int y);
static IEnumerable<Coord> GetNeighbors(this Coord c)
{
	for (int x = c.x - 1; x <= c.x + 1; x++)
	{
		for (int y = c.y - 1; y <= c.y + 1; y++)
		{
			if (x != c.x || y != c.y)
			{
				yield return new Coord(x, y);
			}
		}
	}
}

enum Area
{
	Open,
	Tree,
	Lumberyard,
}

const int size = 50;

Dictionary<Coord, Area> area = File.ReadLines("input.txt")
	.SelectMany(static (l, y) => l.Select((c, x) => (c, coord: new Coord(x, y))))
	.ToDictionary(static p => p.coord, static p => p.c switch { '.' => Area.Open, '|' => Area.Tree, '#' => Area.Lumberyard });
Dictionary<Coord, Area> nextArea = new Dictionary<Coord, Area>();

for (int steps = 0; steps < 10; steps++)
{
	for (int x = 0; x < size; x++)
	{
		for (int y = 0; y < size; y++)
		{
			Coord c = new Coord(x, y);
			var (o, t, l) = c.GetNeighbors().Aggregate((o: 0, t: 0, l: 0), (a, n) =>
				area.TryGetValue(n, out Area aa) ?
					aa switch
					{
						Area.Open => (a.o + 1, a.t, a.l),
						Area.Tree => (a.o, a.t + 1, a.l),
						Area.Lumberyard => (a.o, a.t, a.l + 1),
					} :
					a
				);
			nextArea[c] = area[c] switch
			{
				Area.Open => (t >= 3 ? Area.Tree : Area.Open),
				Area.Tree => (l >= 3 ? Area.Lumberyard : Area.Tree),
				Area.Lumberyard => (l >= 1 && t >= 1 ? Area.Lumberyard : Area.Open),
			};
		}
	}

	(area, nextArea) = (nextArea, area);
	nextArea.Clear();
}

WriteLine(area.Values.Count(static a => a == Area.Tree) * area.Values.Count(static a => a == Area.Lumberyard));
