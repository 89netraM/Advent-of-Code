(string, Dictionary<long, long>) Fold((string mask, Dictionary<long, long> mem) a, string[] s)
{
	if (s[0] == "mask")
	{
		return (s[1], a.mem);
	}
	else
	{
		long hi = Convert.ToInt64(a.mask.Replace("X", "1"), 2);
		long lo = Convert.ToInt64(a.mask.Replace("X", "0"), 2);
		a.mem[Convert.ToInt64(s[0][4..^1])] = (Convert.ToInt64(s[1]) & hi) | lo;
		return a;
	}
}

var result = File.ReadAllLines("input.txt")
	.Select(l => l.Split("=").Select(s => s.Trim()).ToArray())
	.Aggregate((mask: "", mem: new Dictionary<long, long>()), Fold);

WriteLine(
	result.mem.Values.Sum()
);