record Coord(long x, long y);

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

static long Mod(long a, long b) =>
	(a % b + b) % b;

static long ErosionLevel(long geologyIndex, long depth) =>
	Mod(geologyIndex + depth, 20183L);

static Type GeologyType(long erosionLevel) =>
	(Type)Mod(erosionLevel, 3L);

static string[] input = File.ReadAllLines("input.txt");
static long depth = Int64.Parse(input[0].Substring(7));
static Coord target = new Coord(Int64.Parse(input[1].Substring(8).Split(',')[0]), Int64.Parse(input[1].Substring(8).Split(',')[1]));

long riskLevel = Enumerable.Range(0, (int)target.y + 1)
	.SelectMany(y => Enumerable.Range(0, (int)target.x + 1).Select(x => new Coord(x, y)))
	.Select(static c => GeologyType(ErosionLevel(GeologyIndex(c, target), depth)))
	.Cast<long>()
	.Sum();
WriteLine(riskLevel);
