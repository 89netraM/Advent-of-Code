const int MapWidth = 5;
const int MapHeight = 5;

static long CountSetBits(long l)
{
	long count = 0;
	while (l > 0)
	{
		count += l & 1L;
		l >>= 1;
	}
	return count;
}

static int BitPosition(int x, int y) =>
	x + y * MapWidth;

static long IsOccupied(long map, int x, int y) =>
	(map >> BitPosition(x, y)) & 1L;
static long SetOccupied(long map, int x, int y, bool isOccupied) =>
	isOccupied ?
		map | (1L << BitPosition(x, y)) :
		map & ~(1L << BitPosition(x, y));

static (int x, int y)[] Neighbors = new[] { (-1, 0), (0, -1), (1, 0), (0, 1) };
static long NeighborCount(IReadOnlyDictionary<int, long> maps, int level, int x, int y) =>
	Neighbors.Select(n => (x: n.x + x, y: n.y + y))
		.SelectMany(p =>
		{
			if (p.x < 0)
			{
				return new[] { (maps.TryGetValue(level - 1, out long m) ? m : 0L, MapWidth / 2 - 1, MapHeight / 2) };
			}
			else if (MapWidth <= p.x)
			{
				return new[] { (maps.TryGetValue(level - 1, out long m) ? m : 0L, MapWidth / 2 + 1, MapHeight / 2) };
			}
			else if (p.y < 0)
			{
				return new[] { (maps.TryGetValue(level - 1, out long m) ? m : 0L, MapWidth / 2, MapHeight / 2 - 1) };
			}
			else if (MapHeight <= p.y)
			{
				return new[] { (maps.TryGetValue(level - 1, out long m) ? m : 0L, MapWidth / 2, MapHeight / 2 + 1) };
			}
			else if (p.x == MapWidth / 2 && p.y == MapHeight / 2)
			{
				if (x < MapWidth / 2)
				{
					(long, int, int)[] neighbors = new (long, int, int)[MapHeight];
					for (int y = 0; y < neighbors.Length; y++)
					{
						neighbors[y] = (maps.TryGetValue(level + 1, out long m) ? m : 0L, 0, y);
					}
					return neighbors;
				}
				else if (MapWidth / 2 < x)
				{
					(long, int, int)[] neighbors = new (long, int, int)[MapHeight];
					for (int y = 0; y < neighbors.Length; y++)
					{
						neighbors[y] = (maps.TryGetValue(level + 1, out long m) ? m : 0L, MapWidth - 1, y);
					}
					return neighbors;
				}
				else if (y < MapHeight / 2)
				{
					(long, int, int)[] neighbors = new (long, int, int)[MapWidth];
					for (int x = 0; x < neighbors.Length; x++)
					{
						neighbors[x] = (maps.TryGetValue(level + 1, out long m) ? m : 0L, x, 0);
					}
					return neighbors;
				}
				else // if (MapHeight / 2 < y)
				{
					(long, int, int)[] neighbors = new (long, int, int)[MapWidth];
					for (int x = 0; x < neighbors.Length; x++)
					{
						neighbors[x] = (maps.TryGetValue(level + 1, out long m) ? m : 0L, x, MapHeight - 1);
					}
					return neighbors;
				}
			}
			else
			{
				return new[] { (maps.TryGetValue(level, out long m) ? m : 0L, p.x, p.y) };
			}
		})
		.Sum<(long map, int x, int y)>(static p => IsOccupied(p.map, p.x, p.y));

static long Update(IReadOnlyDictionary<int, long> maps, int level)
{
	long map = maps.TryGetValue(level, out long m) ? m : 0L;
	long nextMap = 0;
	for (int y = 0; y < MapHeight; y++)
	{
		for (int x = 0; x < MapWidth; x++)
		{
			if (!(x == MapWidth / 2 && y == MapHeight / 2))
			{
				long neighborCount = NeighborCount(maps, level, x, y);
				nextMap = SetOccupied(
					nextMap, x, y,
					IsOccupied(map, x, y) == 1 ?
						neighborCount == 1 :
						(neighborCount == 1 || neighborCount == 2)
				);
			}
		}
	}
	return nextMap;
}

static string MapToString(long map)
{
	StringBuilder sb = new StringBuilder(MapWidth * MapHeight + MapHeight - 1);
	for (int y = 0; y < MapHeight; y++)
	{
		for (int x = 0; x < MapWidth; x++)
		{
			sb.Append(y == MapHeight / 2 && x == MapWidth / 2 ? '?' : IsOccupied(map, x, y) == 1 ? '#' : '.');
		}
		if (y + 1 < MapHeight)
		{
			sb.AppendLine();
		}
	}
	return sb.ToString();
}

Dictionary<int, long> mapsA = new Dictionary<int, long>
{
	[0] = File.ReadAllBytes("input.txt")
		.Where(static b => b != '\n')
		.Select(static (b, i) => (b, i))
		.Where(static p => p.b == '#')
		.Aggregate(0L, static (m, p) => m | (1L << p.i))
};
Dictionary<int, long> mapsB = new Dictionary<int, long>();
int minLevel = 0;
int maxLevel = 0;

for (int i = 0; i < 200; i++)
{
	for (int level = minLevel - 1; level <= maxLevel + 1; level++)
	{
		long nextMap = Update(mapsA, level);
		if (nextMap != 0L)
		{
			mapsB.Add(level, nextMap);
			if (level < minLevel)
			{
				minLevel--;
			}
			else if (maxLevel < level)
			{
				maxLevel++;
			}
		}
	}
	mapsA.Clear();
	(mapsA, mapsB) = (mapsB, mapsA);
}

WriteLine(mapsA.Sum(static kvp => CountSetBits(kvp.Value)));
