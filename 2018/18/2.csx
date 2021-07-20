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

const int Size = 50;
const int Steps = 1000000000;

Dictionary<Coord, Area> area = File.ReadLines("input.txt")
	.SelectMany(static (l, y) => l.Select((c, x) => (c, coord: new Coord(x, y))))
	.ToDictionary(static p => p.coord, static p => p.c switch { '.' => Area.Open, '|' => Area.Tree, '#' => Area.Lumberyard });
Dictionary<Coord, Area> nextArea = new Dictionary<Coord, Area>();
Dictionary<string, int> seen = new Dictionary<string, int> { [area.ToMapString()] = 0 };

static string ToMapString(this IReadOnlyDictionary<Coord, Area> area)
{
	StringBuilder sb = new StringBuilder();
	for (int y = 0; y < Size; y++)
	{
		for (int x = 0; x < Size; x++)
		{
			sb.Append(area[new Coord(x, y)] switch
			{
				Area.Open => '.',
				Area.Tree => '|',
				Area.Lumberyard => '#',
			});
		}
		sb.AppendLine();
	}
	return sb.ToString();
}

for (int steps = 0; steps < Steps; steps++)
{
	for (int x = 0; x < Size; x++)
	{
		for (int y = 0; y < Size; y++)
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

	if (seen.TryGetValue(nextArea.ToMapString(), out int s))
	{
		steps = Steps - ((Steps - s) % (steps - s));
	}
	else
	{
		seen.Add(nextArea.ToMapString(), steps);
	}
	(area, nextArea) = (nextArea, area);
	nextArea.Clear();
}

WriteLine(area.Values.Count(static a => a == Area.Tree) * area.Values.Count(static a => a == Area.Lumberyard));
