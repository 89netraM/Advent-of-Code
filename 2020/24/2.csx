ISet<(long x, long y)> tiles = new HashSet<(long, long)>();

foreach (string line in File.ReadAllLines("input.txt"))
{
	string sub = line;
	(long x, long y) pos = (0, 0);
	while (sub.Length > 0)
	{
		if (sub.StartsWith("w"))
		{
			pos = (pos.x - 1, pos.y);
			sub = sub.Substring(1);
		}
		else if (sub.StartsWith("e"))
		{
			pos = (pos.x + 1, pos.y);
			sub = sub.Substring(1);
		}
		else if (sub.StartsWith("se"))
		{
			pos = (pos.x + Math.Abs(pos.y % 2), pos.y + 1);
			sub = sub.Substring(2);
		}
		else if (sub.StartsWith("ne"))
		{
			pos = (pos.x + Math.Abs(pos.y % 2), pos.y - 1);
			sub = sub.Substring(2);
		}
		else if (sub.StartsWith("sw"))
		{
			pos = (pos.x - Math.Abs((pos.y + 1) % 2), pos.y + 1);
			sub = sub.Substring(2);
		}
		else if (sub.StartsWith("nw"))
		{
			pos = (pos.x - Math.Abs((pos.y + 1) % 2), pos.y - 1);
			sub = sub.Substring(2);
		}
	}
	if (!tiles.Add(pos))
	{
		tiles.Remove(pos);
	}
}

IEnumerable<(long, long)> Adjacent((long x, long y) pos)
{
	yield return (pos.x - 1, pos.y); // West
	yield return (pos.x + 1, pos.y); // East
	yield return (pos.x + Math.Abs(pos.y % 2), pos.y + 1); // South East
	yield return (pos.x + Math.Abs(pos.y % 2), pos.y - 1); // North East
	yield return (pos.x - Math.Abs((pos.y + 1) % 2), pos.y + 1); // South West
	yield return (pos.x - Math.Abs((pos.y + 1) % 2), pos.y - 1); // North West
}

ISet<(long x, long y)> next = new HashSet<(long, long)>(tiles.Count);
IDictionary<(long x, long y), long> map = new Dictionary<(long, long), long>(tiles.Count);
for (int i = 0; i < 100; i++)
{
	foreach (var pos in tiles)
	{
		foreach (var adj in Adjacent(pos))
		{
			if (map.TryGetValue(adj, out long count))
			{
				map[adj] = count + 1;
			}
			else
			{
				map.Add(adj, 1);
			}
		}
	}

	foreach (var kvp in map)
	{
		if (tiles.Contains(kvp.Key)) // Black
		{
			if (kvp.Value == 1 || kvp.Value == 2)
			{
				next.Add(kvp.Key);
			}
		}
		else // White
		{
			if (kvp.Value == 2)
			{
				next.Add(kvp.Key);
			}
		}
	}
	map.Clear();
	(next, tiles) = (tiles, next);
	next.Clear();
}

WriteLine(
	tiles.Count
);