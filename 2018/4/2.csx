Dictionary<long, long[]> guardSleep = new Dictionary<long, long[]>();
long? currentGuard = null;
int? fell = null;
void Wake(int wake)
{
	if (currentGuard is long cg && fell is int f)
	{
		long[] schedule;
		if (!guardSleep.TryGetValue(cg, out schedule))
		{
			guardSleep[cg] = schedule = new long[60];
		}
		for (int i = f; i < wake; i++)
		{
			schedule[i]++;
		}
		fell = null;
	}
}

foreach (string line in File.ReadLines("input.txt").OrderBy(static l => DateTime.Parse(l[1..17])))
{
	int minute = Int32.Parse(line[15..17]);
	switch (line[19..24])
	{
		case "Guard":
			Wake(minute);
			currentGuard = Int64.Parse(line[26..line.IndexOf(' ', 26)]);
			break;
		case "falls":
			fell = minute;
			break;
		case "wakes":
			Wake(minute);
			break;
	}
}

var sleepiestGuard = guardSleep.OrderByDescending(static kvp => kvp.Value.Max()).First();
WriteLine(sleepiestGuard.Key * sleepiestGuard.Value.Select(static (c, i) => (c, i)).Aggregate(static (a, p) => p.c > a.c ? p : a).i);
