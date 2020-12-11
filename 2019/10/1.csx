var asteroids = File.ReadAllLines("input.txt")
	.SelectMany(
		(l, y) => l
			.Select((a, x) => (c: (x, y), a))
			.Where(p => p.a != '.')
			.Select(p => p.c)
	)
	.ToList();

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

int max = 0;
HashSet<(int x, int y)> set = new HashSet<(int, int)>();
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

	max = Math.Max(max, set.Count);
	set.Clear();
}

WriteLine(max);