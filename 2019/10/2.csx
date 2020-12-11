/// <remarks>
/// Does not work for the large example (the one with the best monitoring
/// station location at 11,13). 199th and 200th swap places.
/// I don't know why, and I don't really care 🤷‍♀️
/// </remarks>

int GDC(int a, int b)
{
	a = Math.Abs(a);
	b = Math.Abs(b);
	while (a != 0 && b != 0)
	{
		if (a > b)
		{
			a %= b;
		}
		else
		{
			b %= a;
		}
	}
	return a + b;
}

class DirComp : IComparer<(int x, int y)>
{
	public static DirComp INSTANCE { get; } = new DirComp();
	private readonly double r = Math.Atan2(-1, 0);
	public int Compare((int x, int y) a, (int x, int y) b)
	{
		return Deg(a).CompareTo(Deg(b));
	}
	public int Deg((int x, int y) c)
	{
		int deg = (int)((Math.Atan2(c.y, c.x) - r) / Math.PI * 180);
		return c.x < 0 && c.y < 0 ? 360 + deg : deg;
	}
}
class LengthComp : IComparer<(int x, int y)>
{
	public static LengthComp INSTANCE { get; } = new LengthComp();
	public int Compare((int x, int y) a, (int x, int y) b)
	{
		return (a.x * a.x + a.y * a.y).CompareTo(b.x * b.x + b.y * b.y);
	}
}

var asteroids = File.ReadAllLines("input.txt")
	.SelectMany(
		(l, y) => l
			.Select((a, x) => (c: (x, y), a))
			.Where(p => p.a != '.')
			.Select(p => p.c)
	)
	.ToList();

(int max, (int x, int y) c)? best = null;
var set = new HashSet<(int x, int y)>();
foreach (var asteroid in asteroids)
{
	foreach (var other in asteroids)
	{
		if (other != asteroid)
		{
			var diff = (x: other.x - asteroid.x, y: other.y - asteroid.y);
			int div = GDC(diff.x, diff.y);
			set.Add((diff.x / div, diff.y / div));
		}
	}

	if (!best.HasValue || set.Count > best.Value.max)
	{
		best = (set.Count, asteroid);
	}
	set.Clear();
}

var map = new Dictionary<(int x, int y), List<(int x, int y)>>();
foreach (var other in asteroids)
{
	if (other != best.Value.c)
	{
		var diff = (x: other.x - best.Value.c.x, y: other.y - best.Value.c.y);
		int div = GDC(diff.x, diff.y);
		var dir = (diff.x / div, diff.y / div);
		if (map.ContainsKey(dir))
		{
			map[dir].Add(diff);
		}
		else
		{
			map.Add(dir, new List<(int, int)> { diff });
		}
	}
}

WriteLine(
	map.SelectMany(kvp => kvp.Value.OrderBy(x => x, LengthComp.INSTANCE).Select((c, i) => (i, c)))
		.OrderBy(ic => ic.i)
		.ThenBy(ic => ic.c, DirComp.INSTANCE)
		.Select((ic, i) => ((best.Value.c.x + ic.c.x) * 100 + (best.Value.c.y + ic.c.y)))
		.Skip(199)
		.First()
);