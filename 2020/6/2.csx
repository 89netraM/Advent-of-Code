int Map(string group)
{
	string[] ss = group.Split("\n").ToArray();
	Dictionary<char, int> set = new Dictionary<char, int>();
	foreach (var item in ss)
	{
		foreach (char c in item)
		{
			if (set.ContainsKey(c))
			{
				set[c]++;
			}
			else
			{
				set.Add(c, 1);
			}
		}
	}

	int count = 0;
	foreach (var item in set)
	{
		if (item.Value == ss.Length) {
			count++;
		}
	}
	return count;
}

WriteLine(
	File.ReadAllText("input.txt")
		.Split("\n\n")
		.Select(Map)
		.Sum()
);