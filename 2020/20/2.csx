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
var image = new Dictionary<(int, int), bool>();
for (int y = 0; y < side; y++)
{
	for (int x = 0; x < side; x++)
	{
		for (int iy = 0; iy < 9; iy++)
		{
			for (int ix = 0; ix < 9; ix++)
			{
				image[(x * 8 + ix, y * 8 + iy)] = result[(x, y)].map[(1 + ix, 1 + iy)];
			}
		}
	}
}
WriteLine("Have image!");

void Rotate()
{
	var temp = new Dictionary<(int, int), bool>();
	for (int x = 0; x < 8 * side; x++)
	{
		for (int y = 8 * side - 1; y >= 0; y--)
		{
			temp[((8 * side - 1) - y, x)] = image[(x, y)];
		}
	}
	image = temp;
}
Dictionary<(int, int), bool> Flip()
{
	var temp = new Dictionary<(int, int), bool>();
	for (int y = 0; y < side * 8; y++)
	{
		for (int x = 0; x < side * 8; x++)
		{
			temp[(x, y)] = image[(x, (8 * side - 1) - y)];
		}
	}
	return temp;
}

bool SeaMonsterAt(int x, int y) =>
	image[(x + 0, y + 1)] &&
	image[(x + 1, y + 2)] &&
	image[(x + 4, y + 2)] &&
	image[(x + 5, y + 1)] &&
	image[(x + 6, y + 1)] &&
	image[(x + 7, y + 2)] &&
	image[(x + 10, y + 2)] &&
	image[(x + 11, y + 1)] &&
	image[(x + 12, y + 1)] &&
	image[(x + 13, y + 2)] &&
	image[(x + 16, y + 2)] &&
	image[(x + 17, y + 1)] &&
	image[(x + 18, y + 1)] &&
	image[(x + 19, y + 1)] &&
	image[(x + 18, y + 0)];
int CountSeaMonsters()
{
	int count = 0;
	for (int y = 0; y < side * 8 - 2; y++)
	{
		for (int x = 0; x < side * 8 - 19; x++)
		{
			if (SeaMonsterAt(x, y))
			{
				count++;
			}
		}
	}
	return count;
}

int count = 0;
for (int i = 0; i < 4; i++)
{
	count = CountSeaMonsters();
	if (count > 0)
	{
		break;
	}
	var temp = Flip();
	(image, temp) = (temp, image);
	count = CountSeaMonsters();
	if (count > 0)
	{
		break;
	}
	image = temp;
	Rotate();
}
WriteLine(image.Sum(kvp => kvp.Value ? 1 : 0) - count * 15);