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

WriteLine(
	tiles.Count
);