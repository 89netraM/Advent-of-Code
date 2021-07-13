Dictionary<char, List<char>> nodes = new Dictionary<char, List<char>>();
List<char> hasPrerequisites = new List<char>();

foreach (string lines in File.ReadLines("input.txt"))
{
	char a = lines[5];
	char b = lines[36];
	List<char> after;
	if (!nodes.TryGetValue(a, out after))
	{
		nodes[a] = after = new List<char>();
	}
	after.Add(b);
	hasPrerequisites.Add(b);
}

SortedSet<char> nexts = new SortedSet<char>(nodes.Keys.Except(hasPrerequisites));
LinkedList<char> sb = new LinkedList<char>();
while (nexts.Count > 0)
{
	char c = nexts.First();
	nexts.Remove(c);
	while (sb.Remove(c));
	sb.AddLast(c);
	if (nodes.TryGetValue(c, out List<char> ns))
	{
		foreach (char n in ns)
		{
			nexts.Add(n);
		}
	}
}
WriteLine(String.Concat(sb));
