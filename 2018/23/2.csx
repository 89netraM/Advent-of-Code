using System.Text.RegularExpressions;

record Coord(long x, long y, long z);
static long ManhattanDistance(this Coord a, Coord b) =>
	Math.Abs(a.x - b.x) + Math.Abs(a.y - b.y) + Math.Abs(a.z - b.z);

record Bot(Coord pos, long r);
static bool InRange(this Bot a, Bot b) =>
	a.pos.ManhattanDistance(b.pos) <= a.r + b.r;

Regex regex = new Regex(@"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)");
Bot[] bots = File.ReadLines("input.txt").Select(line =>
{
	Match match = regex.Match(line);
	return new Bot(new Coord(Int64.Parse(match.Groups[1].Value), Int64.Parse(match.Groups[2].Value), Int64.Parse(match.Groups[3].Value)), Int64.Parse(match.Groups[4].Value));
}).ToArray();

Coord start = new Coord(0, 0, 0);

Dictionary<long, long> dist = new Dictionary<long, long>();
foreach (Bot bot in bots)
{
	long d = bot.pos.x + bot.pos.y + bot.pos.z;
	dist[d - bot.r] = (dist.TryGetValue(d - bot.r, out long min) ? min : 0L) + 1L;
	dist[d + bot.r + 1] = (dist.TryGetValue(d + bot.r + 1, out long max) ? max : 0L) - 1L;
}
long s = 0L;
(long d, long s)[] run = dist.OrderBy(static kvp => kvp.Key).Select(kvp => (kvp.Key, s += kvp.Value)).ToArray();
long max = run.Max(static p => p.s);
(long a, long b)[] intervals = run.Zip(run.Skip(1), static (a, b) => (a, b))
	.Where(p => p.a.s == max)
	.Select(static p => (p.a.d, p.b.d - 1L))
	.ToArray();
if (intervals.Any(static p => p.a <= 0 && p.b >= 0))
{
	WriteLine(0L);
}
else
{
	WriteLine(intervals.Select(static p => p.b < 0 ? -p.b : p.a).Min());
}
