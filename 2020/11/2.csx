Dictionary<(int x, int y), bool> result = File.ReadAllText("input.txt")
	.Split("\n")
	.SelectMany(
		(l, y) => l
			.Select((a, x) => (c: (x, y), a))
			.Where(p => p.a != '.')
			.Select(p => (p.c, a: p.a == '#'))
	)
	.ToDictionary(p => p.c, p => p.a);

bool CheckDir((int x, int y) p, (int x, int y) d)
{
	(int x, int y) c = (p.x + d.x, p.y + d.y);
	while (!result.ContainsKey(c) && 0 <= c.x && c.x < 95 && 0 <= c.y && c.y < 92)
	{
		c = (c.x + d.x, c.y + d.y);
	}
	return (0 <= c.x && c.x < 95 && 0 <= c.y && c.y < 92) && result[c];
}
int CountSounding((int x, int y) p)
{
	int count = 0;
	for (int x = -1; x <= 1; x++)
	{
		for (int y = -1; y <= 1; y++)
		{
			if ((x, y) != (0, 0) && CheckDir(p, (x, y)))
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
		int sounding = CountSounding(kvp.Key);
		if (kvp.Value && sounding >= 5)
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