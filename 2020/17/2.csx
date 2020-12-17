Dictionary<(int x, int y, int z, int w), bool> result = File.ReadAllText("input.txt")
	.Split("\n")
	.SelectMany(
		(l, y) => l
			.Select((a, x) => (c: (x, y, 0, 0), a))
			.Where(p => p.a != '.')
			.Select(p => (p.c, a: p.a == '#'))
	)
	.ToDictionary(p => p.c, p => p.a);

int CountSounding((int x, int y, int z, int w) p, Dictionary<(int x, int y, int z, int w), bool> map)
{
	int count = 0;
	for (int x = p.x - 1; x <= p.x + 1; x++)
	{
		for (int y = p.y - 1; y <= p.y + 1; y++)
		{
			for (int z = p.z - 1; z <= p.z + 1; z++)
			{
				for (int w = p.w - 1; w <= p.w + 1; w++)
				{
					if ((x, y, z, w) != p && map.ContainsKey((x, y, z, w)) && map[(x, y, z, w)])
					{
						count++;
					}
				}
			}
		}
	}
	return count;
}

IEnumerable<(int x, int y, int z, int w)> PositionsInResult()
{
	foreach (var kvp in result)
	{
		var p = kvp.Key;
		yield return p;
		for (int x = p.x - 1; x <= p.x + 1; x++)
		{
			for (int y = p.y - 1; y <= p.y + 1; y++)
			{
				for (int z = p.z - 1; z <= p.z + 1; z++)
				{
					for (int w = p.w - 1; w <= p.w + 1; w++)
					{
						if ((x, y, z, w) != p && !result.ContainsKey((x, y, z, w)))
						{
							yield return (x, y, z, w);
						}
					}
				}
			}
		}
	}
}

Dictionary<(int x, int y, int z, int w), bool> next = new Dictionary<(int x, int y, int z, int w), bool>();
for (int i = 0; i < 6; i++)
{
	foreach (var p in PositionsInResult())
	{
		int sounding = CountSounding(p, result);
		bool v = result.ContainsKey(p) && result[p];
		if (v && !(sounding == 2 || sounding == 3))
		{
			next[p] = false;
		}
		else if (!v && sounding == 3)
		{
			next[p] = true;
		}
		else
		{
			next[p] = v;
		}
	}

	(result, next) = (next, result);
	next.Clear();
}

WriteLine(
	result.Sum(x => x.Value ? 1 : 0)
);