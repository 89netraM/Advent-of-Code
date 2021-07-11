const int MapWidth = 5;
const int MapHeight = 5;

static int BitPosition(int x, int y) =>
	x + y * MapWidth;

static long IsOccupied(long map, int x, int y) =>
	(map >> BitPosition(x, y)) & 1L;
static long SetOccupied(long map, int x, int y, bool isOccupied) =>
	isOccupied ?
		map | (1L << BitPosition(x, y)) :
		map & ~(1L << BitPosition(x, y));

static (int x, int y)[] Neighbors = new[] { (-1, 0), (0, -1), (1, 0), (0, 1) };
static long NeighborCount(long map, int x, int y) =>
	Neighbors.Select(n => (x: n.x + x, y: n.y + y))
		.Where(static p =>
			0 <= p.x && p.x < MapWidth && 0 <= p.y && p.y < MapHeight
		)
		.Sum(p => IsOccupied(map, p.x, p.y));

static long Update(long map)
{
	long nextMap = 0;
	for (int y = 0; y < MapHeight; y++)
	{
		for (int x = 0; x < MapWidth; x++)
		{
			long neighborCount = NeighborCount(map, x, y);
			nextMap = SetOccupied(
				nextMap, x, y,
				IsOccupied(map, x, y) == 1 ?
					neighborCount == 1 :
					(neighborCount == 1 || neighborCount == 2)
			);
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
			sb.Append(IsOccupied(map, x, y) == 1 ? '#' : '.');
		}
		if (y + 1 < MapHeight)
		{
			sb.AppendLine();
		}
	}
	return sb.ToString();
}

long map = File.ReadAllBytes("input.txt")
	.Where(static b => b != '\n')
	.Select(static (b, i) => (b, i))
	.Where(static p => p.b == '#')
	.Aggregate(0L, static (m, p) => m | (1L << p.i));
HashSet<long> seenMaps = new HashSet<long> { map };

for (int i = 1; true; i++)
{
	map = Update(map);
	if (!seenMaps.Add(map))
	{
		WriteLine(map);
		return;
	}
}
