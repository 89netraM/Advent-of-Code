#r "nuget: OptimizedPriorityQueue, 5.1.0"
using Priority_Queue;

record Coord(long x, long y);
static IEnumerable<Coord> GetNeighbors(this Coord coord)
{
	yield return new Coord(coord.x - 1, coord.y);
	yield return new Coord(coord.x, coord.y + 1);
	yield return new Coord(coord.x + 1, coord.y);
	yield return new Coord(coord.x, coord.y - 1);
}

static Dictionary<Coord, long> cache = new Dictionary<Coord, long>();
static long GeologyIndex(Coord c, Coord target)
{
	if (c.x == 0L && c.y == 0L) return 0L;
	if (c.x == target.x && c.y == target.y) return 0L;
	if (c.y == 0L) return c.x * 16807L;
	if (c.x == 0L) return c.y * 48271L;
	if (cache.TryGetValue(c, out long i)) return i;
	return cache[c] = ErosionLevel(GeologyIndex(new Coord(c.x - 1L, c.y), target), depth) * ErosionLevel(GeologyIndex(new Coord(c.x, c.y - 1L), target), depth);
}

enum Type : long
{
	Rocky = 0,
	Wet = 1,
	Narrow = 2,
}
static bool IsEquipmentAllowed(this Type t, Equipment e) => t switch
{
	Type.Rocky => e == Equipment.ClimbingGear || e == Equipment.Torch,
	Type.Wet => e == Equipment.ClimbingGear || e == Equipment.Neither,
	Type.Narrow => e == Equipment.Torch || e == Equipment.Neither,
};
static Equipment OtherEquipment(this Type t, Equipment e) => (t, e) switch
{
	(Type.Rocky, Equipment.ClimbingGear) => Equipment.Torch,
	(Type.Rocky, Equipment.Torch) => Equipment.ClimbingGear,
	(Type.Wet, Equipment.ClimbingGear) => Equipment.Neither,
	(Type.Wet, Equipment.Neither) => Equipment.ClimbingGear,
	(Type.Narrow, Equipment.Torch) => Equipment.Neither,
	(Type.Narrow, Equipment.Neither) => Equipment.Torch,
};

static long Mod(long a, long b) =>
	(a % b + b) % b;

static long ErosionLevel(long geologyIndex, long depth) =>
	Mod(geologyIndex + depth, 20183L);

static Type GeologyType(long erosionLevel) =>
	(Type)Mod(erosionLevel, 3L);

enum Equipment
{
	Neither,
	Torch,
	ClimbingGear,
}

static string[] input = File.ReadAllLines("input.txt");
static long depth = Int64.Parse(input[0].Substring(7));
static Coord start = new Coord(0L, 0L);
static Coord target = new Coord(Int64.Parse(input[1].Substring(8).Split(',')[0]), Int64.Parse(input[1].Substring(8).Split(',')[1]));

HashSet<(Coord, Equipment)> visited = new HashSet<(Coord, Equipment)>();
SimplePriorityQueue<(Coord, long, Equipment), long> toVisit = new SimplePriorityQueue<(Coord, long, Equipment), long>();
toVisit.Enqueue((start, 0L, Equipment.Torch), 0L);

while (toVisit.Count > 0)
{
	var (current, time, equipment) = toVisit.Dequeue();
	if (visited.Add((current, equipment)))
	{
		Type type = GeologyType(ErosionLevel(GeologyIndex(current, target), depth));
		foreach (var (nextC, nextT, nextE) in current.GetNeighbors().Select(c => (c, time + 1, equipment)).Append((current, time + 7, type.OtherEquipment(equipment))))
		{
			if (nextC.x >= 0 && nextC.y >= 0 &&
				!visited.Contains((nextC, nextE)) &&
				GeologyType(ErosionLevel(GeologyIndex(nextC, target), depth)).IsEquipmentAllowed(nextE))
			{
				if (current == target && equipment == Equipment.Torch)
				{
					WriteLine(time);
					return;
				}
				else
				{
					toVisit.Enqueue((nextC, nextT, nextE), nextT);
				}
			}
		}
	}
}
