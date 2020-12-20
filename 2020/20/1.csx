using System.Collections.Immutable;

(long number, Dictionary<(int, int), bool> map) MakeTile(string t)
{
	var arr = t.Split("\n").ToArray();
	long number = Int64.Parse(arr[0][5..^1]);
	Dictionary<(int, int), bool> map = arr[1..]
		.SelectMany((l, y) => l.Select((c, x) => ((x, y), c == '#')))
		.ToDictionary(p => p.Item1, p => p.Item2);
	return (number, map);
}

(long number, Dictionary<(int, int), bool> map) FlipTile((long number, Dictionary<(int, int), bool> map) t)
{
	Dictionary<(int, int), bool> map = new Dictionary<(int, int), bool>();
	for (int y = 0; y < 10; y++)
	{
		for (int x = 0; x < 10; x++)
		{
			map[(x, y)] = t.map[(x, 9 - y)];
		}
	}
	return (t.number, map);
}
(long number, Dictionary<(int, int), bool> map) RotateTile((long number, Dictionary<(int, int), bool> map) t)
{
	Dictionary<(int, int), bool> map = new Dictionary<(int, int), bool>();
	for (int x = 0; x < 10; x++)
	{
		for (int y = 9; y >= 0; y--)
		{
			map[(9 - y, x)] = t.map[(x, y)];
		}
	}
	return (t.number, map);
}
IEnumerable<(long number, Dictionary<(int, int), bool> map)> AllVariants((long number, Dictionary<(int, int), bool> map) t)
{
	for (int i = 0; i < 4; i++)
	{
		yield return t;
		yield return FlipTile(t);
		t = RotateTile(t);
	}
}

bool MatchRight((long number, Dictionary<(int, int), bool> map) l, (long number, Dictionary<(int, int), bool> map) r)
{
	for (int y = 0; y < 10; y++)
	{
		if (l.map[(9, y)] != r.map[(0, y)])
		{
			return false;
		}
	}
	return true;
}
bool MatchBelow((long number, Dictionary<(int, int), bool> map) a, (long number, Dictionary<(int, int), bool> map) b)
{
	for (int x = 0; x < 10; x++)
	{
		if (a.map[(x, 9)] != b.map[(x, 0)])
		{
			return false;
		}
	}
	return true;
}

var tiles = File.ReadAllText("input.txt")
	.Split("\n\n")
	.Select(MakeTile)
	.ToArray();

int side = (int)Math.Sqrt(tiles.Length);

ImmutableDictionary<(int, int), (long number, Dictionary<(int, int), bool> map)> Solve(ImmutableDictionary<(int, int), (long number, Dictionary<(int, int), bool> map)> image, ImmutableHashSet<long> usedTiles, int x, int y)
{
	foreach (var t in tiles)
	{
		if (!usedTiles.Contains(t.number))
		{
			foreach (var vt in AllVariants(t))
			{
				if ((!image.ContainsKey((x - 1, y)) || MatchRight(image[(x - 1, y)], vt)) &&
					(!image.ContainsKey((x, y - 1)) || MatchBelow(image[(x, y - 1)], vt)))
				{
					var nextImage = image.SetItem((x, y), vt);
					if (x + 1 == side && y + 1 == side)
					{
						return nextImage;
					}
					else
					{
						int nextX = x + 1;
						int nextY = y;
						if (nextX == side)
						{
							nextX = 0;
							nextY++;
						}
						var ret = Solve(nextImage, usedTiles.Add(vt.number), nextX, nextY);

						if (!(ret is null))
						{
							return ret;
						}
					}
				}
			}
		}
	}
	return null;
}

var result = Solve(ImmutableDictionary.Create<(int, int), (long number, Dictionary<(int, int), bool> map)>(), ImmutableHashSet.Create<long>(), 0, 0);
for (int y = 0; y < side; y++)
{
	for (int x = 0; x < side; x++)
	{
		Write($"{result[(x, y)].number}    ");
	}
	WriteLine();
}
WriteLine(
	result[(0, 0)].number *
	result[(0, side - 1)].number *
	result[(side - 1, 0)].number *
	result[(side - 1, side - 1)].number
);