int Map(string group) {
	HashSet<char> set = new HashSet<char>();
	foreach (var item in group.Split("\n"))
	{
		foreach (char c in item)
		{
			set.Add(c);
		}
	}
	return set.Count;
}

WriteLine(
	File.ReadAllText("input.txt")
		.Split("\n\n")
		.Select(Map)
		.Sum()
);