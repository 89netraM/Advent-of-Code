IEnumerable<(long, long)> AllPossible(string mask)
{
	return Perms(mask)
		.Select(aa => (
			Convert.ToInt64(RemoveXs(aa.Replace("0", "1")), 2),
			Convert.ToInt64(RemoveXs(aa), 2)
		));

	static string RemoveXs(string s) => s.Replace("x", "0").Replace("X", "1");

	static IEnumerable<string> Perms(string mask)
	{
		if (!String.IsNullOrWhiteSpace(mask))
		{
			foreach (string m in Perms(mask[1..]))
			{
				if (mask[0] == 'X')
				{
					yield return "x" + m;
					yield return "X" + m;
				}
				else
				{
					yield return mask[0] + m;
				}
			}
		}
		else {
			yield return "";
		}
	}
}

(IEnumerable<(long, long)>, Dictionary<long, long>) Fold((IEnumerable<(long hi, long lo)> mask, Dictionary<long, long> mem) a, string[] s)
{
	if (s[0] == "mask")
	{
		return (AllPossible(s[1]), a.mem);
	}
	else
	{
		foreach (var mask in a.mask)
		{
			a.mem[(Convert.ToInt64(s[0][4..^1]) & mask.hi) | mask.lo] = Convert.ToInt64(s[1]);
		}
		return a;
	}
}

var result = File.ReadAllLines("input.txt")
	.Select(l => l.Split("=").Select(s => s.Trim()).ToArray())
	.Aggregate((mask: Enumerable.Empty<(long, long)>(), mem: new Dictionary<long, long>()), Fold);

WriteLine(
	result.mem.Values.Sum()
);