Dictionary<(int x, int y), bool> result = File.ReadAllText("input.txt")
	.Split("\n")
	.SelectMany(
		(l, y) => l
			.Select((a, x) => (c: (x, y), a))
			.Where(p => p.a != '.')
			.Select(p => (p.c, a: p.a == '#'))
	)
	.ToDictionary(p => p.c, p => p.a);

int CountSounding((int x, int y) p, Dictionary<(int x, int y), bool> map)
{
	int count = 0;
	for (int x = p.x - 1; x <= p.x + 1; x++)
	{
		for (int y = p.y - 1; y <= p.y + 1; y++)
		{
			if ((x, y) != p && map.ContainsKey((x, y)) && map[(x, y)])
			{
				count++;
			}
		}
	}
	return count;
}

Dictionary<(int x, int y), bool> next = new Dictionary<(int x, int y), bool>();
while (true)
{
	foreach (var kvp in result)
	{
		int sounding = CountSounding(kvp.Key, result);
		if (kvp.Value && sounding >= 4)
		{
			next[kvp.Key] = false;
		}
		else if (!kvp.Value && sounding == 0)
		{
			next[kvp.Key] = true;
		}
		else
		{
			next[kvp.Key] = kvp.Value;
		}
	}

	if (result.SequenceEqual(next))
	{
		break;
	}

	(result, next) = (next, result);
	next.Clear();
}

WriteLine(
	result.Sum(x => x.Value ? 1 : 0)
);